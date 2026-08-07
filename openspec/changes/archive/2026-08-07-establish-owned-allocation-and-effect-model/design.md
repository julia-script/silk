## Context

See `proposal.md` for motivation and the delta specs for observable requirements. The compiler
already has Flow syntax and typed failures, capability roles and provision, `Usize`, target-aware
layout, affine ownership facts, a structured MIR DAG, evaluator execution, native LLVM, and direct
Wasm. The active but unimplemented scoped-allocation plan assumed named destination scopes, dynamic
cleanup records, and arena-backed results.

The Effect pattern corpus and three adversarial reviews established two boundaries. First, lazy
imperative effects, typed failure, requirements, capture-derived reuse, and deterministic affine
cleanup compose cleanly. Second, allowing ordinary values to retain arbitrary provider storage would
require a complete static dependency-transformer system covering returns, mutable output places,
generics, callbacks, and separate compilation. Bootstrap does not need that system to obtain owned
growable output.

## Goals / Non-Goals

**Goals:**

- Make Effect the one public lazy-computation abstraction and state exactly which code is eager.
- Make repeatability, retry, and provider lifecycle consequences derive from ordinary ownership.
- Add the smallest self-contained owned-memory substrate capable of implementing `Vector<T>` in Silk.
- Preserve one compiler-owned target-aware structured DAG across evaluator, LLVM, and Wasm.
- Keep allocator implementations, collection policy, and resource wrappers in Silk/stdlib wherever
  unsafe primitives suffice.

**Non-Goals:**

- Arena-backed results that escape their arena, provider-dependent values, stored borrows, hidden
  dependency sets, `depends on`, or a Rust-like lifetime solver.
- Named `Scope`, dynamic heterogeneous finalizers, `defer`, `errdefer`, or general brackets in the
  language kernel.
- Fallible or asynchronous automatic cleanup, cancellation, race, fork, task groups, or streams.
- Shared/interior-mutable allocator access, user-facing FFI, a stable allocator ABI, or arbitrary
  user allocator implementations in the first executable slice.
- Backward compatibility with Flow spelling or the unimplemented scoped-allocation proposal.

## Decisions

### 1. `effect {}` is primitive; `effect fn` is sugar

An ordinary function executes its ordinary statements at call time. `effect { ... }` captures its
environment and delays the imperative block. An `effect fn` behaves as though an ordinary function
returned an effect block containing its complete body.

This distinction makes mixed eager and lazy construction explicit without requiring generator
syntax:

```silk
fn parseLater(source: own Bytes) -> Effect<Syntax ! ParseError ? &mut Allocator> {
  let options = ParseOptions.default()

  return effect {
    return run Parser.parse(move source, options)
  }
}
```

`options` is constructed eagerly; parsing is delayed. Statically known Effect composition lowers
directly to HIR/MIR and does not imply a universal runtime interpreter object.

Every `effect {}` construction site also defines a hidden nominal Effect instance. The instance owns
a compiler-generated capture environment whose target layout is selected with the other concrete
instances. Copy captures are stored snapshots, shared and exclusive captures store ordinary borrow
provenance, and moved captures become owned environment fields with deterministic Drop. A generated
runner receives that environment and executes the lazy body. If the Effect is never run, its owned
captures still clean when the Effect value cleans.

The public structural `Effect<Success ! Failure ? Requirements>` contract does not expose this
identity, but the compiler preserves it through elaboration, instance discovery, layout, and MIR.
Different construction sites therefore do not become interchangeable runtime objects merely because
their public Effect contracts match. Joins that would erase distinct hidden identities require an
explicit future erasure/sum design and are rejected in this slice. This is closure conversion, not a
universal Effect interpreter or allocator/runtime privilege.

The alternative—keeping `flow fn` as a special function class without an expression boundary—was
rejected because it cannot clearly express eager setup returning a lazy imperative computation.

### 2. Captures derive execution access and retry legality

The compiler records each Effect capture using ordinary access modes:

- a Copy value is snapshotted at construction;
- a shared borrow permits repeated shared execution;
- an exclusive borrow requires exclusive execution and its mutations persist between runs;
- a moved affine owner belongs to the Effect; consuming it makes that Effect take-once.

`Effect.retry` requires repeatable capture state. It reconstructs body locals for every attempt but
does not reset captures. This supports explicit retry counters while rejecting accidental cloning of
owned inputs. A separate reusable/single-shot Effect type was rejected because it duplicates facts
already present in ownership.

### 3. Typed failures are detached owned values

Failure payloads may own scalars, paths, strings, vectors, and other self-contained resources. They
may not retain lexical borrows or provider-dependent storage. `OutOfMemory` remains allocation-free.
Copy failures use `fail value`; affine failures transfer with `fail move value`.

This replaces the earlier named-scope escape rule. Restricting every error to static text was
rejected as unnecessary once owned allocation is self-contained. Allowing provider-backed errors
was deferred because failure propagation would then require the same dependency system as successful
results.

### 4. Existing provision captures; `provideWith` acquires per execution

`Capability.provide` captures an existing provider by borrow or move. A borrow follows its lexical
owner; a moved provider follows the lifetime and execution mode of the resulting Effect. Provision
alone does not promise cleanup before an outer handler.

`Capability.provideWith` is the per-run acquisition boundary. Each execution acquires a fresh affine
provider owner, runs the inner Effect, then drops that owner after success or typed failure. Therefore
an outer `Effect.catch` observes cleanup first only when the protected wrapper actually owns an
acquisition. No cleanup is promised after a trap.

### 5. Allocation results own reclaim authority

The bootstrap allocator operation is conceptually:

```silk
interface Allocator {
  effect fn allocate(layout: Layout) -> Allocation
    ! OutOfMemory
}
```

The requirement uses exclusive access in the MVP (`? &mut Allocator`) because Silk has not designed
safe shared interior mutation. The borrow is an access loan only and ends when the allocation call
returns. The returned `Allocation` is affine and contains a private reclaim ticket: release entry,
stable context when required by the approved runtime, layout/identity facts, and active state. It does
not contain a safe borrow of the provider.

`SystemAllocator` is a stateless or process/module-root-backed stdlib provider whose allocation
tickets remain valid independently of the provider value. Native tickets call the compiler-versioned
aligned release shim. Wasm tickets address the module-owned heap manager and carry the block metadata
needed for deterministic logical release. A deterministic failing provider wraps this contract for
tests without changing returned ownership.

Passing an allocator as ambient global state was rejected because allocation requirements must stay
visible. Retaining a provider pointer in every allocation was rejected because it makes the provider
a hidden lifetime dependency. Named destination scopes and cleanup registrations were rejected
because they create a second cleanup authority beside the affine owner.

### 6. Arena is an ordinary future allocator implementation

The compiler, HIR, MIR, evaluator, and backends never branch on allocator kind. A future
`ArenaAllocator` must satisfy a general public allocator contract like any other stdlib type.

A traditional arena whose outputs become invalid on reset cannot honestly implement the bootstrap
self-contained-allocation contract unless a library representation keeps its backing state alive or
a non-escaping API prevents outputs from escaping. General provider-dependent validity may be added
later, but only as a type-system feature applicable to allocators, files, cursors, transactions, and
other resources equally. Arena itself receives no privilege.

### 7. `RawBuffer<T>` and uninitialized slots are the unsafe seam

`Layout` is validated target-aware bytes and alignment. Typed repeated layout retains canonical `T`,
runtime count, stride, checked total bytes, and target width. An affine `RawBuffer<T>` combines an
`Allocation` with that typed layout. Lexical unsafe slot projections check bounds and prevent the
buffer from moving while a place is live.

Unsafe Silk code maintains the initialized-prefix and aliasing invariants. The compiler does not add
a runtime bitmap or collection-shaped initialization primitive. A small uninitialized value/slot API
supports write, read/move, drop-in-place, and bulk copy/move only where element properties allow it.

The older four-layer `Layout`/`SlotLayout<T>`/`Allocation`/`Slot<T>` public design should be kept as
small as implementation evidence permits. `RawBuffer<T>` and its unsafe uninitialized operations are
the intended stdlib-facing depth; private compiler/runtime records need not become source types.

### 8. Drop is the sole automatic cleanup authority

An affine nominal type may define one restricted Drop hook. It is synchronous, infallible,
non-allocating, requirement-free, unable to move from `self`, and runs before automatic field
cleanup. Locals clean in reverse acquisition order; fields retain the fixed declaration-defined
order already published by ownership. Explicit `drop value` consumes early.

The exact exit table is:

| Exit | Automatic Drop |
| --- | --- |
| Fallthrough, `return`, `break`, `continue` | Yes |
| Typed failure propagation | Yes |
| Trap/defect | No bootstrap guarantee |
| Cancellation/interruption | Not present in bootstrap |

Safe external acquisition must establish an affine owner immediately inside the trusted unsafe shim;
a copyable raw handle with an outstanding cleanup obligation cannot cross into safe code. Cleanup
whose failure matters remains an explicit consuming API such as `close` or `flush`, followed by an
infallible Drop fallback. General `defer`, dynamic finalizer stacks, and async cleanup hooks were
rejected for MVP because no admitted workload requires them.

### 9. Vector is the proof that the primitive seam is deep enough

`Vector<T>` owns `RawBuffer<T>`, length, and capacity in Silk. Its Drop hook destroys initialized
elements before the backing buffer field cleans. Growth performs a transactional sequence:

1. compute checked capacity and typed layout;
2. allocate a replacement buffer;
3. move or copy initialized elements while a guard tracks the committed prefix;
4. on failure, Drop rolls back the replacement prefix and preserves the original vector;
5. on success, swap/commit state and drop the old buffer.

Append, reserve, insert, remove, pop, and truncate are ordinary generic actor operations. `Vector`
does not store an allocator; operations that may grow require one explicitly. An Iterable protocol,
primitive resize, implicit default allocator, and compiler-owned vector behavior remain excluded.

### 10. Layout is selected before the backend and MIR stays a DAG

Concrete instance discovery precedes one target-layout plan. The plan fixes `Usize`, allocation
layout, strides, raw-buffer and Vector shape, Effect outcome transport, reclaim tickets, and Drop
calling shapes. HIR remains target-independent; MIR carries the selected plan and represents Effect,
allocation, initialization, failure, loops, and cleanup as an acyclic structured control graph.

The evaluator is the semantic oracle. LLVM may flatten MIR into a cyclic CFG; Wasm may translate it
to structured control. Neither backend reconstructs source structure, chooses layout, changes OOM to
a trap, or recognizes SystemAllocator/ArenaAllocator by type name.

### 11. Deferred resource dependencies remain documented, not half-implemented

The rejected hidden-dependency sketch would need a callable transformation summary for return values
and every mutable/out parameter, symbolic dependency polymorphism for generics and callbacks,
generative resource roots, invalidation rules, and separate-compilation metadata. A rare return-only
`depends on` annotation would be unsound for operations such as appending a dependent value into a
caller-owned Vector.

The design is preserved in the Effect corpus as a future stress test, but none of those metadata,
syntax, or restrictions enter this change. If an arena-backed escaping workload later earns the
feature, it begins as a new explicit type-system proposal rather than an allocator patch.

## Risks / Trade-offs

- [The Flow-to-Effect rename touches every compiler phase and fixture] → Land the semantic rename
  vertically and reject mixed vocabulary; do not retain compatibility aliases during alpha.
- [Unsafe initialized-prefix code can leak or double-drop] → Keep the raw API lexical and narrow,
  model it in the evaluator, sweep every allocation/initialization failure point, and verify cleanup
  plans before backend emission.
- [A reclaim ticket accidentally depends on provider storage] → Restrict the first implementations
  to stable system/module-root release contexts and verify allocation remains valid after the
  provider borrow ends.
- [Wasm logical release may not immediately return linear-memory pages] → Require deterministic
  logical ownership and reusable heap blocks, not impossible page shrinking; expose physical policy
  only through implementation-specific metrics.
- [Drop hooks grow into general finalizers] → Enforce the closed restrictions in semantics,
  ownership, HIR, MIR verification, evaluator, and negative fixtures.
- [The combined change is large] → Implement in ordered vertical gates with validation after Effect
  rename, allocation ownership, raw buffers/Drop, Vector, and three-engine acceptance.

## Migration Plan

1. Rename Flow syntax, models, actor calls, fixtures, highlighters, and labs to Effect while preserving
   current typed-failure behavior.
2. Add `effect {}` and capture/retry/provider semantics; establish three-engine Effect parity.
3. Add validated layout, self-contained Allocation, SystemAllocator, Drop, and raw typed storage
   through semantic/evaluator acceptance before backend work.
4. Realize the same contract in native and Wasm from the compiler-owned layout plan.
5. Implement `Vector<T>` in Silk, run complete failure sweeps, and add compiler-shaped examples to
   unified labs.
6. Run strict OpenSpec validation and repository release checks. If parity fails, roll back the
   allocation/raw-buffer/Vector surface together; do not restore named scopes or allocator-specific
   lifetime shortcuts.
