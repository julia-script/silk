## Context

Every claim below was measured against `main` at 30310de, and the mechanism was validated with a
throwaway spike that is **not part of this change**. Where a measurement contradicts issue #19, the
measurement is stated first and the issue's text is treated as superseded.

The gate is one sentence: a struct must be able to reach itself through a value whose _layout_ does
not depend on the struct's layout, and the value behind that indirection must still be released
exactly once at runtime.

## Goals / Non-Goals

Goals:

- One explicit heap indirection a program asks for by name, so `SyntaxTree`, `Hir.Expression`,
  `Type.Type`, and `Mir` become expressible.
- Cycle detection that matches the rule `bootstrap-struct-types` already states — a cycle "of inline
  struct fields" — rather than the stricter rule the implementation currently enforces.
- A runtime release path with no new compiler machinery, proven by a leak test that fails today.

Non-Goals: inline recursion (stays rejected), `Rc`/`Arc`/shared ownership, cyclic graphs, an arena
allocator (#22), and any relaxation of stack-depth behaviour — a deep chain exhausts the stack, and
that is accepted.

## Measurements

### The SCC check does not break cycles through intrinsic nominals

`Type.nominals` (`Type.ts:694-713`) recurses into `self.arguments`:

```ts
isNominal(self) ? Object.freeze([self, ...self.arguments.flatMap(nominals)]) : ...
```

so `RawBuffer<Node>` yields `[RawBuffer, Node]`. Neighbour extraction at
`DeclarationIndex.ts:2784-2791` maps those to `module.name` and filters by `byKey.has`; `RawBuffer`
is dropped because it has no struct declaration, but `Node` is kept and the self-edge survives.
A second, independent site — the `selfEdge` test at `DeclarationIndex.ts:3685-3693` — repeats the
same `Type.nominals` call, so **both** sites must change together.

Measured on `main`:

| source                                                                         | diagnostics                              |
| ------------------------------------------------------------------------------ | ---------------------------------------- |
| `struct Node { next: RawBuffer<Node> }`                                        | `["SEM0020"]`, `dependency: Unavailable` |
| `struct Node { next: Slot<Node> }`                                             | `["SEM0020", "SEM0054"]`                 |
| `struct Cell<T> { buffer: RawBuffer<T> }` + `struct Node { next: Cell<Node> }` | `["SEM0020"]`                            |
| `struct Pair<T> { value: T }` + `struct Node { next: Pair<Node> }`             | `["SEM0020"]`                            |
| `struct Node { anchor: [Node; 0] }`                                            | `["SEM0020"]`                            |

The issue's requirement 5 ("accepted with no change to that check") is therefore false, and its
premise — that `RawBuffer` and `Slot` already break cycles — is false as well.

### An intrinsic nominal cannot carry a `Drop` hook

`Ownership.cleanupPlan` (`Ownership.ts:1617-1726`) reaches a hook only through two gates:

- `Ownership.ts:1673-1680` — `NoCleanup` unless `DeclarationIndex.byCanonical` yields a
  `StructDeclaration`.
- `Ownership.ts:1703-1706` — `structPlan` unless `DeclarationIndex.witness` yields a
  `SourceConformanceWitness`, i.e. a source-level `impl Drop`.

`RawBuffer` and `Slot` are entries in `Type.intrinsicNominals` (`Type.ts:287-300`) with no struct
declaration and no `impl` anywhere in the standard library. So issue requirement 1 (intrinsic
nominal) and requirement 4 (a source `Drop` hook) are mutually exclusive under the current
architecture. This is the fork; it is resolved below.

### The hook rules the drop path must satisfy

`DeclarationIndex.ts:3429-3465` requires the hook to be named `drop`, `Ordinary`, with zero type
parameters, exactly one parameter named `self` of type `&mut Provider`, returning `()`, with
`failureRow.failures.length === 0` and `requirementRow.requirements.length === 0`.

`Box.make` must allocate through `Allocator` and fail with `OutOfMemoryError`, which looks like a
conflict — but it is not, and `Vector<T>` already shows why. Allocation lives in `append`
(`vector.silk:97`, `! OutOfMemoryError ? &mut Allocator`) while the hook at `vector.silk:64-72` only
_releases_, and releasing needs no allocator and cannot fail. `Box` inherits that split exactly.

### The runtime recursion vehicle already exists and is already exercised

`RawBufferCleanup` (`Ownership.ts:1636-1645`) releases the allocation and **does not descend into
the element type**. That single fact is what makes the static plan finite through an indirection,
and it is also why the payload must be dropped explicitly. `Vector` does exactly that, via
`Slot.dropValue` (`slot.silk:16-19` → `Intrinsic.slotDrop`) in `releaseBuffer`
(`vector.silk:81-93`).

`Slot.dropValue<T>` is not a black box: the MIR `SlotDrop` operation carries its own
`cleanup: CleanupPlan` for `T` (`Mir.ts:1508-1509`), lowered by Wasm at `WasmBackend.ts:1353-1377`
and by LLVM through `dropThroughPlan` at `Backend.ts:3539-3573`, and
`Instances.slotDropHookTargets` (`Instances.ts:542-567`) discovers the hook instances that plan
calls. So the recursion is:

```
cleanup(Tree)          = StructCleanup { shape: UnionCleanup { Branch: HookCleanup(Box.drop<Tree>) x2 } }
Box.drop<Tree>  body   → Slot.dropValue<Tree> → SlotDrop { cleanup: cleanup(Tree) } → call Box.drop<Tree>
```

The static plan is finite because `HookCleanup` lowers to a **call**, not an inlining, and because
`RawBufferCleanup` terminates the descent. The unbounded part happens at runtime, inside a
recursive function — which is precisely requirement 7's "deep chain exhausts the stack".

The `seen` guard at `Ownership.ts:1671-1672` (which returns `NoCleanup` and is the mechanism the
issue warns about) is therefore never reached on this shape. That was verified by trace, not by
argument.

### PR #92 is the enabling precedent, not the cleanup path

PR #92 (`b8a22e2`) replaced `throw new RangeError('Wasm cleanup does not yet lower hook-bearing
union cases')` with tag-guarded per-case lowering at `WasmBackend.ts:1233-1257` and `1353-1377`.
`Box`'s storage is a vacant/occupied union, the same idiom `Vector` uses, so that lowering is what
makes the shape safe in Wasm generally. Note precisely: for `Box<T>` itself the union's occupied
case is hook-free in every instantiation, because `RawBufferCleanup` does not descend into `T`. #92
is what removes the hazard for a hook-bearing member reached through the same union idiom; it is
not a path this change extends with new code.

### Polymorphic recursion is already rejected

`struct Bad<T> { value: T next: Box<Bad<Box<T>>> }` would demand an infinite tower of drop
instances. Instantiating it (via `Vector.make<Bad<i32>>()`, which forces `cleanupPlan(Bad<i32>)`
through `Slot.dropValue`) produces `["SEM0053"]` — `Diagnostic.polymorphicRecursionCode`, raised
from `Instances.ts:237` by the existing finite-discovery check. No new guard is needed; a test pins
it.

## Decisions

### `Box<T>` is standard-library Silk source, not a compiler intrinsic

This is the decision issue #19 got wrong, and the one Julia should review most closely. Both options
were worked through; the recommendation is **B**.

**Option A — compiler-intrinsic nominal (the issue's requirement 1).** `Box` joins
`Type.intrinsicNominals` with a `Layout` branch beside `RawBuffer`. The cycle fix is then small: a
barrier list of indirecting intrinsics that neighbour extraction refuses to descend through. But
because an intrinsic has no struct declaration and no source `impl`, it cannot carry a `Drop` hook,
so cleanup needs a new `BoxCleanup` plan node **plus** a compiler-synthesized per-instantiation drop
thunk (an intrinsic cannot inline `T`'s cleanup at the plan level without re-entering the `seen`
guard and leaking), **plus** lowering for both in `WasmBackend` and the LLVM `Backend`. That is new
release machinery in two backends, written to be correct on first contact, guarding the exact leak
this issue exists to prevent.

**Option B — ordinary stdlib struct over `RawBuffer<T>` (recommended).**

```silk
struct Vacant {}
struct Occupied<T> { buffer: RawBuffer<T> }
pub struct Box<T> { storage: Vacant | Occupied<T> }

impl<T> Drop for Box<T> {
  fn drop(self: &mut Box<T>) -> () {
    let storage = Intrinsic.replace(self.storage, Vacant {})
    return match move storage {
      Vacant nothing => ()
      Occupied<T> full => releaseOccupied<T>(move full)
    }
  }
}
```

`Vacant` exists for the same reason `Vector`'s `Empty<T>` does: the hook holds `&mut self` and
`Intrinsic.replace` needs something to put back. It is a non-generic marker rather than
`Vector`'s `anchor: [T; 0]`, because a zero-length array of `T` is still an inline reach into `T`
(measured: `struct Node { anchor: [Node; 0] }` is `SEM0020` today, and this change deliberately
leaves that conservative). `Box` never needs to borrow an empty slice, so it needs no anchor.

Cost of B: the cycle fix must recognise that `Box`'s parameter is _not_ reached inline, which cannot
be a spelling check — `AGENTS.md` forbids recognising a library declaration by name in semantic
analysis. It must be a general per-parameter analysis. That analysis is the whole implementation
cost, and it buys something A does not: any user type that indirects through `RawBuffer` becomes
recursion-capable for free, without asking the compiler's permission.

Why B wins:

- **It needs no new release machinery.** Measured end-to-end (below): zero new plan nodes, zero
  backend changes, balanced releases on all three engines. A's synthesized drop thunk is exactly the
  code most likely to reintroduce the leak.
- **`AGENTS.md` "Minimal compiler privilege" points at it directly**: "A new compiler feature
  exposes only the smallest target-neutral primitive needed to build its public API in ordinary Silk
  source. Keep … safe reusable wrappers in the standard library." `RawBuffer` is that primitive and
  it already exists; `Box` is the safe reusable wrapper.
- **It makes the implementation match the spec it already has.** `bootstrap-struct-types` says a
  cycle "consisting only of inline struct fields" is rejected. The implementation over-approximates
  "inline" as "mentioned anywhere in the field's type". B's analysis is the honest reading of the
  existing sentence, not a new rule.

What B costs relative to the issue: requirement 1 as literally written ("compiler-intrinsic
nominal") is not met. Requirement 1's _outcome_ — one pointer-sized heap indirection, `MoveOnly`,
allocating through `Allocator` — is met in full. If Julia wants the intrinsic spelling for a reason
not captured here (a stable ABI symbol, a debugger contract, a plan to special-case `Box` in later
optimisation), Option A is still available, and the cycle-detection work in section 1 of `tasks.md`
shrinks rather than disappears while section 3 grows into two-backend work.

### Cycle detection uses inline reach, computed as a fixed point

Replace the `Type.nominals` call at the two cycle sites with an inline-reach walk. Rules:

- `RawBuffer<T>` and `Slot<T>`: yield the nominal, **do not** descend into arguments. Their layouts
  are `{$allocation, count}` and `{$address}` (`Layout.ts:644-651`) — element-independent.
- Any other nominal `S<A₀…Aₙ>`: yield `S`, and descend into `Aᵢ` only when parameter `i` of `S` is
  reached inline by `S`'s own fields.
- Fixed arrays, slices, references, callables, effects, and unions: descend as `Type.nominals` does
  today, preserving current diagnostics exactly.

"Parameter `i` of `S` is inline" is a monotone fixed point: start with no parameter inline, and
repeatedly mark parameter `i` inline when some field of `S` reaches it under the rules above.
Growth-only, so it terminates.

`Type.nominals` itself is **not** changed. The reported `dependency` list at
`DeclarationIndex.ts:3706-3714` keeps naming every referenced nominal, so
`bootstrap-struct-types`'s "the field retains that type dependency" stays true. Only cycle detection
switches graphs.

### The runtime cleanup mechanism, stated plainly

For a value reachable only through a `Box`:

1. The holder's static plan contains `HookCleanup { hook: Box.drop<T>, inner: … }` — one call, at a
   constant offset, exactly as `WasmBackend.ts:1189-1204` requires.
2. `Box.drop<T>` moves the storage out through `&mut self`, matches the union, and calls
   `Slot.dropValue<T>` on the single element.
3. `SlotDrop` carries `T`'s own cleanup plan, which contains the next `HookCleanup` call.
4. Depth is consumed by the call stack, not by the plan. Deep chains exhaust the stack; nothing
   leaks.

If step 2 were omitted, `RawBufferCleanup` would release the block and abandon its contents. That is
the silent leak, and it is why the tests below are part of the deliverable rather than a follow-up.

## Validation

A throwaway spike on `main` (30310de) implemented the fixed point at both cycle sites and wrote
`Box` in test source. **Not shipped; recorded here as evidence the design works.**

Cycle detection, after the change:

| source                                                                         | result                            |
| ------------------------------------------------------------------------------ | --------------------------------- |
| `struct Node { next: RawBuffer<Node> }`                                        | accepted, `dependency: Available` |
| `struct Cell<T> { buffer: RawBuffer<T> }` + `struct Node { next: Cell<Node> }` | accepted                          |
| `struct Pair<T> { value: T }` + `struct Node { next: Pair<Node> }`             | `SEM0020` — unchanged             |
| `struct Node { next: Node }`                                                   | `SEM0020` — unchanged             |
| `struct Node { anchor: [Node; 0] }`                                            | `SEM0020` — unchanged             |

End-to-end, a 3-level tree (`Tree` → `Branch` → `Box<Tree>`, 7 nodes, 6 boxes):

| engine                       | result                                                                   |
| ---------------------------- | ------------------------------------------------------------------------ |
| diagnostics                  | none                                                                     |
| evaluator                    | `Completed`, value 42, **6 `AllocationAcquire` / 6 `AllocationRelease`** |
| Wasm (`silk_main`, release)  | 42                                                                       |
| native LLVM (clang, release) | exit 42                                                                  |

Negative control — byte-identical source with the `impl Drop for Box` deleted:

|             |                                                    |
| ----------- | -------------------------------------------------- |
| diagnostics | **none**                                           |
| evaluator   | `Completed`, value 42, **6 acquires / 2 releases** |
| Wasm        | 42                                                 |

The leak is silent, produces the right answer, and passes every check the compiler has today. Only
the acquire/release trace catches it.

Regression: the full compiler suite ran with the cycle change applied — **149 files, 1160 tests,
all passing**.

## Risks

- **Two cycle sites, one rule.** `DeclarationIndex.ts:2784-2791` and `:3685-3693` must use the same
  walk and the same fixed point. Changing one and not the other yields a graph that disagrees with
  itself. Tasks 1.2 and 1.3 pin both.
- **The fixed point widens what compiles.** Any user generic that indirects through `RawBuffer`
  becomes recursion-capable, not just `Box`. That is intended and is the reason the rule is general
  rather than a `Box` special case, but it is a real surface increase and the reviewer should agree
  to it explicitly.
- **A hook that forgets `Slot.dropValue` leaks silently.** Nothing in the type system prevents it.
  The mitigation is the negative-control test (task 4.3), which must be written as a test that
  _fails_ if the hook is removed.
- **`[T; 0]` stays an inline reach.** Conservative and unchanged, which is why `Box` uses a
  non-generic `Vacant`. Revisiting it would let `Vector`'s `Empty<T>` shape work inside recursive
  types too, but that is a separate change.

## Open question for review

One, already answered with a recommendation rather than left blocking: **intrinsic `Box` (A) or
stdlib `Box` (B)?** The recommendation is B, on the evidence above. Nothing else in this design
forks on it — the cycle work in section 1 is required either way, only its shape changes.
