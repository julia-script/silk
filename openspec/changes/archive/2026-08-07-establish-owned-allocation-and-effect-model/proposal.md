## Why

Silk can borrow runtime-sized input and express lazy typed failure, but it still cannot construct
owned growable output. The previous allocation plan coupled ownership to named runtime scopes and
dynamic cleanup records; adversarial review showed that this duplicated affine cleanup and led
toward an implicit lifetime-effect system before the bootstrap compiler needs one.

## What Changes

- **BREAKING** Rename the public language abstraction from Flow to Effect: `flow fn`, `Flow<A>`, and
  `Flow.*` become `effect fn`, `Effect<A>`, and `Effect.*`.
- Add `effect { ... }` as the primitive lazy imperative boundary; define `effect fn` as sugar for a
  function whose entire body is delayed.
- Define capture-derived execution and retry: shared captures are repeatable, mutable captures are
  exclusively repeatable with persistent state, and consumed affine captures make an Effect
  one-shot and ineligible for retry.
- Clarify that provision of an existing provider captures it but does not create a per-run cleanup
  boundary; `provideWith` owns per-execution acquisition and Drop.
- Add target-aware validated `Layout`, an affine self-contained `Allocation`, unsafe typed
  `RawBuffer<T>`/uninitialized-slot primitives, explicit consuming `drop`, and a restricted
  synchronous infallible `Drop` hook.
- Keep allocator access explicit through capability requirements. `SystemAllocator` is the first
  implementation; allocation results carry their reclaim authority and never retain or rediscover
  the provider.
- Implement `Vector<T>` and its growth/rollback behavior entirely in Silk above those primitives.
- Require identical ownership, failure, cleanup, and layout behavior across evaluator, native LLVM,
  and direct Wasm, exposed through the unified `/labs` inspector.
- Remove named scope wrappers, allocation cleanup registries, `depends on`, hidden resource-
  dependency sets, and allocator-specific lifetime rules from the bootstrap design.
- Defer arena-backed escaping values, dynamic finalizers, `defer`/`errdefer`, fallible or asynchronous
  automatic cleanup, cancellation, and concurrency. A future arena remains ordinary standard-library
  code and receives no compiler privilege.

## Capabilities

### New Capabilities

- `bootstrap-owned-allocation`: Valid layouts, self-contained affine allocation, typed raw buffers,
  restricted Drop, explicit early drop, SystemAllocator, and Silk-written Vector behavior.

### Modified Capabilities

- `bootstrap-flow-functions`: Rename Flow to Effect and add effect-expression, capture, retry, and
  provider-boundary semantics.
- `bootstrap-syntax`: Parse, recover, traverse, and format the Effect and owned-allocation surface.
- `bootstrap-semantic-facts`: Publish Effect contracts, allocation types, Drop restrictions, and
  typed allocation failure.
- `bootstrap-ownership`: Enforce Effect capture modes, affine allocation, initialized-buffer moves,
  and deterministic cleanup.
- `bootstrap-hir`: Retain effect construction/execution, allocation, initialization, and Drop without
  backend vocabulary.
- `bootstrap-instances`: Discover generic buffer, Vector, Effect, and cleanup instances
  deterministically.
- `bootstrap-target-layout`: Plan target-width layouts, typed buffers, Effect results, and cleanup
  calling shapes before lowering.
- `bootstrap-mir`: Preserve effect control, typed failure, allocation, initializedness, and cleanup
  in the target-aware structured DAG.
- `bootstrap-evaluation`: Serve as the semantic oracle for laziness, retries, allocation failures,
  ownership transfer, rollback, and Drop order.
- `bootstrap-backend`: Realize the same self-contained allocation and Effect contract in LLVM and
  Wasm without backend-selected layout or lifetime policy.
- `bootstrap-compiler-driver`: Require three-engine parity, deterministic failure sweeps, and
  post-failure reuse.
- `bootstrap-syntax-inspector`: Add coordinated Effect, ownership, allocation, layout, MIR, and
  execution examples to the unified `/labs` inspector.

## Impact

- Breaking source and public compiler-model rename from Flow to Effect across compiler packages,
  fixtures, formatter, highlighters, documentation, and labs.
- New compiler work across syntax, analysis, ownership, HIR, monomorphization, target layout, MIR,
  evaluator, native LLVM, direct Wasm, and the private runtime shim.
- New bootstrap standard-library foundations for raw owned storage and `Vector<T>`; collections stay
  Silk code rather than compiler intrinsics.
- Wayfinder issues 01, 02, 03, 07, 08, 09, the bootstrap map, and the syntax prototype now record the
  replacement design. The Effect pattern corpus remains research evidence and marks scope-heavy
  scenarios as deferred stress cases.
