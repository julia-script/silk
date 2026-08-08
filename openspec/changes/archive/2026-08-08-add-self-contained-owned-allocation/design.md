## Context

See `proposal.md` for motivation and the delta specs for observable requirements. The compiler
already has target-selected `Usize` and `Layout`, lazy Effects with typed failure and requirement
rows, capability roles and provision, affine moves and lexical loans, first-class callables,
compiler-owned HIR/MIR, an evaluator oracle, native LLVM, direct Wasm, deterministic artifact
encoders, and one public `Analysis` facade. Explicit `drop value` already crosses the implemented
pipeline for ordinary owner cleanup; restricted user Drop hooks, allocation, raw storage, and a
heap do not. The current provisional `SystemAllocator.make()` erases directly to the intrinsic
`Allocator` value; this change replaces that shortcut with a nominal provider and conformance
witness.

Wayfinder fixes the semantic boundary: successful allocation is a self-contained affine owner,
allocator implementations receive no compiler privilege, unsafe code is explicit, automatic
cleanup is synchronous and infallible, and named lifetime scopes or provider-dependent values are
not part of bootstrap. The archived scope-based allocation plan is historical input only and must
not be revived.

## Goals / Non-Goals

**Goals:**

- Establish a small safe allocator contract whose implementations can be written without
  allocator-kind branches in the compiler.
- Put the unavoidable C-like authority in a narrow unsafe adoption and typed-slot seam while making
  successful Allocation ownership safe and deterministic.
- Extend the existing compiler representations vertically rather than adding a parallel allocation
  pipeline or runtime interpreter.
- Make the substrate deep enough for the following change to implement `Vector<T>` entirely in
  Silk.

**Non-Goals:**

- Implement `Vector<T>`, `Bytes`, `String`, a scanner, bulk memory helpers, allocator metrics, or an
  iterable abstraction in this change.
- Support provider-dependent escaping storage, a resettable arena contract, named lifetime scopes,
  stored borrows, hidden dependency sets, or `depends on`.
- Add shared ownership, interior-mutable allocator access, implicit/default allocation, public
  `free`, primitive resize, implicit zeroing, a runtime initializedness bitmap, or collection-aware
  compiler operations.
- Add fallible or asynchronous automatic cleanup, `defer`, `errdefer`, cancellation, unwinding
  traps, or a stable external allocator ABI.

## Decisions

### 1. Allocation extends the existing vertical compiler spine

The frontend adds source nodes and semantic facts in the existing lexer/parser/elaboration actors.
Ownership remains the only producer of loans and cleanup plans. HIR stays target-neutral; instance
discovery closes over allocator witnesses, concrete typed storage, and Drop hooks; target layout
selects physical shapes once; lowering emits verified MIR; evaluator, LLVM, and Wasm consume that
same plan. `Analysis` projects the new artifacts to `/labs` without exposing engine-private state.

This follows the current clusters around parsing, elaboration, ownership, lowering, MIR verification,
evaluation, and backend emission. A separate memory IR or allocation inspector was rejected because
it would duplicate phase authority and make layout or cleanup disagree with the rest of the
compiler.

### 2. Unsafe is a lexical boundary; allocation remains an ordinary qualified call

Wayfinder's concrete surface uses an explicit `unsafe { ... }` boundary. Inside it, allocation and
slot operations keep ordinary qualified actor-call syntax:

```silk
effect fn reserve<T>(layout: Layout, count: Usize) -> RawBuffer<T>
  ! OutOfMemory
  ? &mut Allocator
{
  unsafe {
    let allocation = run Allocator.allocate(layout)
    return RawBuffer.from<T>(move allocation, count)
  }
}
```

`unsafe` grants permission; it does not disable type checking, bounds checks, ownership, target
layout validation, or typed failure. It acknowledges the remaining initializedness and aliasing
obligations. Allocation does not gain bespoke punctuation, and no syntax names an allocator
implementation or lifetime scope.

The alternative of prefixing each call with `unsafe` was rejected because Wayfinder says unsafe
operations remain ordinary qualified calls inside a boundary. An unsafe function kind can be added
later if repeated evidence warrants it; the bootstrap boundary expression is sufficient now.

### 3. The safe Allocator contract returns an independent Allocation

Conceptually, the capability operation is:

```silk
interface Allocator {
  effect fn allocate(layout: Layout) -> Allocation
    ! OutOfMemory
}
```

The requirement is exclusive in bootstrap (`? &mut Allocator`) because safe shared interior
mutation is not designed. The access loan ends when the operation returns. `OutOfMemory` is a
zero-allocation owned failure and a rejected request creates neither a raw block nor a cleanup
obligation.

`Allocation` is affine and contains a private active reclaim ticket. The compiler-selected physical
shape includes the logical base, requested `Layout`, a noncapturing reclaim entry, an optional raw
context, and active state. The entry has an infallible requirement-free contract over the context,
base, and layout. Moving the Allocation moves this ticket; Drop calls it once; no later capability
lookup occurs.

An unsafe allocator-author operation adopts an acquired raw block into `Allocation`. It accepts
only a statically known noncapturing reclaim function plus an optional raw context, giving every
target a fixed ticket shape without requiring existential boxed callables or hidden allocation.
Unsafe allocator implementations are responsible for keeping that context valid until reclaim and
for establishing the owner immediately after external acquisition. This is the same kind of
obligation as constructing a safe resource wrapper around a C handle.

Retaining `&mut Allocator` in each result was rejected because it turns a temporary access loan into
a hidden lifetime dependency. A general capturing reclaim callable was rejected for bootstrap
because `Allocation` would then require unknown-sized callable erasure or another allocation before
allocation itself works. The raw-context form remains sufficient for user-authored unsafe allocator
implementations.

### 4. SystemAllocator is standard-library policy over target platform primitives

`SystemAllocator` satisfies the same nominal `Allocator` conformance as any other implementation.
Its actor operation invokes a compiler-versioned platform primitive, then adopts the resulting block
with the platform release entry. Semantic analysis, HIR, MIR, evaluator, and backends see ordinary
witness dispatch; only the target platform primitive differs.

The source declaration keeps implementation and capability separate:

```silk
struct SystemAllocator {}

impl Allocator for SystemAllocator {
  allocate: SystemAllocator.allocate
}
```

The mapping names an existing qualified actor operation; it does not define a hidden method.
`SystemAllocator.make()` returns nominal `SystemAllocator`, and
`Allocator.provide(effect, &mut system)` selects its conformance witness for the requested role.
The same declaration works for user-authored quota, pool, or other unsafe providers. This is a
breaking correction to the current implementation-erased intrinsic value, which is acceptable
during alpha.

Native uses private aligned acquire/release shim operations. Direct Wasm uses module-owned heap
functions with checked alignment, a deterministic reusable free list, and checked `memory.grow`;
growth failure becomes `OutOfMemory`. Logical release need not shrink linear memory pages. Static
data, private slice frames, and heap metadata receive one non-overlapping compiler-planned memory
partition.

A deterministic quota allocator used by tests is an ordinary conformance that delegates successful
requests and rejects chosen ordinals. It demonstrates that dispatch is not hard-coded to
SystemAllocator. A future arena receives no special rule: it must make every returned Allocation
independent—perhaps through its own stable control block—or expose a non-escaping API.

### 5. RawBuffer owns bytes; Slot is a lexical unsafe place

`Layout` remains the validated Copy byte-size/alignment value. `Layout.of<T>()` uses the selected
concrete layout of `T`; `Layout.repeat(element, count)` performs checked aligned-stride and target-
width multiplication before an allocator runs. Invalid alignment and overflow remain ordinary
validation data rather than `OutOfMemory`.

`RawBuffer<T>` is an affine standard-library-facing owner containing one Allocation and one runtime
logical count. Its unsafe construction verifies that the Allocation's recorded Layout equals the
compiler-selected repeated layout for canonical `T` and that count. The compiler need not expose a
second public `SlotLayout<T>` value merely to retain facts already present in the RawBuffer type,
count, Allocation layout, and target plan.

`RawBuffer.slot(&mut buffer, index)` produces a non-escaping exclusive `Slot<T>` place after an
ordinary runtime bounds check. Unsafe Slot operations are the minimum set:

- write a value into storage the caller promises is uninitialized;
- take/move a value from storage the caller promises is initialized;
- destroy in place a value the caller promises is initialized.

The slot loan prevents the buffer from moving or dropping while the place is live. Safe code may
own and drop a RawBuffer, but cannot read uninitialized `T`. Unsafe code tracks initialized prefixes
using ordinary fields and guards. Bulk `memcpy`, `memmove`, `memset`, property-gated copying, and a
runtime bitmap are deferred until the Vector/scanner workload proves which primitives are needed.

### 6. Restricted Drop is a compiler-sealed conformance

The concrete declaration form follows Wayfinder:

```silk
impl Drop for Guard<Token> {
  fn drop(self: &mut Guard<Token>) -> Unit {
    while self.initialized > 0 {
      self.initialized = self.initialized - 1
      unsafe {
        Slot.drop(RawBuffer.slot(&mut self.buffer, self.initialized))
      }
    }
  }
}
```

`Drop` is compiler-sealed rather than an ordinary callable interface. An affine nominal type may
have exactly one hook; Copy types may not. The hook is synchronous, has no success value beyond
`Unit`, no failure or requirement row, cannot allocate, cannot move from or replace `self`, cannot
let a self borrow escape, and cannot be called directly. It may mutate scalar bookkeeping and use
qualified unsafe storage operations.

The compiler inserts the hook before fixed declaration-order field cleanup. Locals clean in reverse
acquisition order. `drop value` consumes early and uses the same sequence. Cleanup runs on
fallthrough, `return`, `break`, `continue`, and typed failure propagation. Traps abort without a
cleanup guarantee. There is no second dynamic cleanup registry to arm or disarm.

### 7. Target layout fixes private shapes before MIR

The layout plan adds canonical shapes for Allocation tickets, RawBuffer values, Slot addresses,
Drop calls, and typed allocation outcomes after instance discovery. Address and `Usize` lanes follow
the selected target. A zero-byte allocation still has one logical affine identity and one cleanup
obligation; its physical base may be a shared sentinel because pointer identity is not a safe
ownership identity.

MIR operations refer to logical allocation and ticket identities and remain in the existing acyclic
region graph. Verification checks type/layout provenance, capability witness contracts, loans,
consumption, hook restrictions, and exactly-once cleanup. It does not claim that an unsafe slot is
initialized. LLVM may flatten regions into a CFG and Wasm may nest them, but neither reconstructs
layout, lifetime, or cleanup policy.

### 8. The evaluator and acceptance corpus prove failure behavior first

The evaluator assigns deterministic per-run logical allocation, slot, and reclaim identities. A
requested failure ordinal rejects acquisition atomically. Successful blocks retain logical bytes and
initialized values independently of JavaScript references; Drop events are explicit and GC is
irrelevant.

The canonical fixture is an affine construction guard over runtime-counted move-only elements. It
exercises zero and nonzero counts, padded and over-aligned element layouts, successful transfer,
explicit early drop, return, `break`, `continue`, typed failure, and a trap that promises no unwind.
Failure sweeps reject every allocation ordinal and exit after every initialized prefix. After each
failure, another run in the same evaluator and module must succeed.

Native, Wasm, and evaluation compare values, typed failures, logical initialization/destruction,
hook-before-field order, and exactly-once release. Fresh-process gates compare every textual and
binary artifact. `/labs` projects the same fixtures through `Analysis`; it does not create a special
allocation inspector or imply that Vector already exists.

## Risks / Trade-offs

- [Unsafe adoption can encode an invalid reclaim context] → Keep it inside an explicit unsafe
  boundary, require a noncapturing static reclaim entry and raw context, document the exact
  validity contract, and make all safe stdlib constructors establish ownership atomically.
- [Restricted Drop may accidentally become a general finalizer] → Validate its closed contract in
  semantics, ownership, HIR, MIR, and negative fixtures; do not add failure, services, allocation,
  capture, or direct invocation.
- [RawBuffer initializedness bugs can leak or double-drop] → Keep Slot lexical and bounds-checked,
  use ordinary affine construction guards, and sweep every partial prefix without claiming that
  safe analysis proves unsafe runtime state.
- [Wasm release cannot return pages to the host] → Specify logical reuse and exactly-once ownership,
  not page shrinking, and test reusable blocks after exhaustion.
- [A fixed raw-context reclaim ticket is less ergonomic than a capturing callable] → Accept the
  explicit unsafe burden for allocator authors until general owned callable erasure exists; keep
  safe allocator consumers unaware of the ticket.
- [The vertical change is large] → Implement in reviewable gates: syntax/types, safe allocator and
  evaluator, Drop/raw storage, target/MIR, native, Wasm, then differential acceptance.

## Migration Plan

1. Add syntax, formatting, canonical types, and semantic facts while keeping downstream artifacts
   explicitly unavailable.
2. Add allocator witnesses, Allocation ownership, restricted Drop, and RawBuffer/Slot rules through
   HIR and ownership; keep allocation-free programs unchanged.
3. Extend instance discovery, target layout, MIR, verification, and deterministic encodings.
4. Establish evaluator success, exhaustion, rollback, and cleanup as the semantic oracle.
5. Add native shim/SystemAllocator realization, then the direct Wasm module heap, consuming the same
   MIR plan.
6. Add three-engine failure sweeps, unified Labs presets, and fresh-process determinism gates.

If parity cannot be established, remove the new allocation/adoption, RawBuffer/Slot, and restricted
Drop surface together. Do not fall back to named scopes, retained allocator borrows, ambient
allocation, backend-selected layout, or allocator-kind branches.
