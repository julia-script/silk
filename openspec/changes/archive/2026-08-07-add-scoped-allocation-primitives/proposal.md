## Why

Silk can borrow runtime-sized input, but it still cannot create owned output whose size emerges while
a compiler pass runs. The next smallest memory boundary is explicit scoped allocation: enough to
construct safe owned values with deterministic reclamation, without making collections or allocator
policy compiler magic.

## What Changes

- Add a copyable validated `Layout` value with target-aware `Usize` size and alignment, plus
  `SlotLayout<T>` with checked runtime count and aligned stride; invalid alignment and
  representational overflow remain ordinary validation data.
- Add explicit named destination scopes and allocator requirements. No allocator is ambient or
  selected merely because one provider is visible, and allocator providers must outlive allocations
  made through them.
- Add one unsafe fallible allocation primitive that accepts only a validated layout and produces an
  affine owned allocation carrying a private, unforgeable reclaim ticket for its originating
  allocator.
- Register allocation cleanup with its destination scope and perform it exactly once in LIFO order on
  normal return, typed failure, early return, `break`, `continue`, and explicit consuming `drop`.
- Add unsafe lexical `Slot<T>` place operations over a matching `SlotLayout<T>`. The compiler checks
  type, provenance, bounds, and allocation lifetime; unsafe Silk code owns dynamic initializedness
  and aliasing invariants rather than receiving a hidden bitmap or dependent-type solver.
- Add one restricted compiler-invoked drop hook for affine structs. It is infallible,
  non-allocating, requirement-free, cannot move from `self`, runs before field cleanup, and lets a
  Silk-written collection drop its initialized elements before its `Allocation` field releases bytes.
- Add typed `OutOfMemory` propagation independently of the allocator requirement and keep automatic
  reclaim typed-infallible, non-allocating, and independent of ambient services.
- Extend HIR, ownership facts, target-aware layout, the structured MIR DAG, evaluator, native LLVM,
  direct WebAssembly, determinism fixtures, and the unified `/labs` inspector with the same allocation
  and cleanup semantics.
- Keep `Vector<T>` and all collection behavior in future Silk standard-library code built from these
  primitives. This change adds no resize primitive, public `free`, implicit zeroing, safe raw
  allocation, stored slice, bulk memory operation, general user finalizer, arena policy, or
  allocation-metrics surface.
- Depend on the separately delivered executable foundations for pointer-sized integers, flow
  functions and typed failures, capability roles/provision and hidden service slots, and named
  `Scope.scoped` wrappers. This change MUST NOT emulate those foundations with allocator-specific
  syntax, enums, ambient callbacks, traps, or backend-owned ABI choices.

## Capabilities

### New Capabilities

- `bootstrap-scoped-allocation`: Valid byte and typed-slot layouts, explicit allocator-and-scope
  allocation, affine byte ownership, origin-bound reclaim, unsafe slot access, `OutOfMemory`,
  restricted drop hooks, and deterministic exactly-once cleanup.

### Modified Capabilities

- `bootstrap-syntax`: Parse and recover qualified unsafe slot operations, restricted drop-hook
  declarations, and explicit consuming-drop operations on top of the established flow/scope surface.
- `bootstrap-semantic-facts`: Publish canonical layout/allocation/slot types, scope identities,
  allocator requirements, initialization transitions, and typed allocation failures.
- `bootstrap-ownership`: Enforce affine allocation ownership, scope outlives rules, lexical slot
  borrows, restricted drop-hook rules, consuming drop, and LIFO exactly-once byte cleanup.
- `bootstrap-hir`: Retain explicit scope, allocation, initialization, and cleanup operations with
  canonical provenance and no backend vocabulary.
- `bootstrap-instances`: Discover concrete generic allocation and slot operations and their cleanup
  behavior deterministically.
- `bootstrap-target-layout`: Plan target-width validated layouts, addresses, typed slots, and cleanup
  calling shapes before MIR lowering or backend emission.
- `bootstrap-mir`: Represent scopes, allocation failure, initialization transitions, drops, and
  cleanup registration in the target-aware structured control DAG.
- `bootstrap-evaluation`: Model deterministic allocator providers, affine storage, unsafe slot state,
  failure injection, and ordered reclamation.
- `bootstrap-backend`: Realize the compiler-planned allocation and cleanup contract consistently in
  native LLVM and direct WebAssembly without choosing layout independently.
- `bootstrap-compiler-driver`: Require evaluator, native, and Wasm parity for success, injected
  exhaustion, early exits, cleanup order, and post-failure reuse.
- `bootstrap-syntax-inspector`: Expose scoped-allocation syntax, facts, ownership, layout, MIR,
  execution traces, and backend realization through coordinated `/labs` projections.

## Impact

- Compiler syntax, semantic analysis, ownership and cleanup planning, HIR, instance discovery,
  target layout, MIR and verifier, evaluator, native backend, and Wasm backend under
  `packages/compiler`.
- Compiler-owned runtime helpers and private native/Wasm allocation machinery; no stable external ABI
  or general user-facing FFI is introduced.
- Three-engine fixtures, failure-injection and fresh-process determinism tests, and `/labs` presets and
  projections under `apps/docs`.
- The next change can implement `Vector<T>` entirely in Silk and use it to prove borrowed source bytes
  flowing into owned growable tokens.
