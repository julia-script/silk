## Why

Silk can borrow runtime-sized compiler input, but it still cannot create runtime-sized owned output.
The language now has the prerequisite pieces—`Usize`, target-aware `Layout`, typed Effects, explicit
capability provision, affine ownership, and first-class callables—so the next coherent boundary is
real allocation and deterministic reclamation without compiler-known allocators or lifetime scopes.

## What Changes

- Add the ordinary `Allocator` capability and its first standard-library implementation,
  `SystemAllocator`, with one fallible validated-layout allocation operation.
- **BREAKING**: Stop erasing `SystemAllocator` into an `Allocator` value. Providers retain their
  nominal type and satisfy capability requirements through ordinary conformance witnesses, so a
  user-authored unsafe allocator can participate without an allocator-kind branch.
- Represent successful allocation as a self-contained affine `Allocation` carrying private,
  unforgeable reclaim authority; it remains valid after the provider borrow ends and releases
  exactly once through automatic cleanup.
- Make allocation exhaustion the allocation-free typed failure `OutOfMemory`, preserving ordinary
  Effect failure and requirement propagation.
- Add the narrow unsafe typed-storage substrate needed by future Silk collections: raw owned-buffer
  projection, lexical uninitialized slots, checked layout provenance, explicit initialization, and
  deterministic rollback.
- Add restricted synchronous infallible `Drop`, automatic cleanup on every structured exit, and
  explicit consuming `drop` for early cleanup. Traps continue to provide no cleanup guarantee.
- Carry allocation, reclaim, raw-storage, and Drop semantics through syntax, semantic facts,
  ownership, HIR, instances, layout planning, MIR, evaluation, LLVM, direct Wasm, driver
  determinism, and the unified `/labs` inspector.
- Keep allocator policy outside the compiler: there is no allocator-kind tag, arena privilege,
  named lifetime `Scope`, dynamic finalizer registry, ambient allocator, public `free`, primitive
  resize, zero-fill promise, or compiler-known collection.
- Leave `Vector<T>` and the scanner proof to the next change; this change provides and proves only
  the substrate from which they can be written in Silk.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-owned-allocation`: Make the accepted allocator, self-contained owner, unsafe storage,
  and Drop contracts executable while preserving the no-privilege boundary.
- `bootstrap-syntax`: Define lossless, recoverable syntax for allocation requirements, unsafe typed
  storage, restricted Drop declarations, and explicit early drop.
- `bootstrap-semantic-facts`: Publish canonical allocator dispatch, allocation ownership, storage
  initialization, Drop, and `OutOfMemory` facts without provider-retention or allocator-kind facts.
- `bootstrap-ownership`: Enforce affine allocations, lexical raw-storage projections, initialization
  obligations, restricted Drop, and exactly-once cleanup on every structured exit.
- `bootstrap-hir`: Represent validated allocation, typed storage transitions, explicit drop, and
  automatic Drop in canonical target-neutral HIR.
- `bootstrap-instances`: Discover allocator witnesses, reclaim functions, Drop hooks, and concrete
  storage operations through the existing finite monomorphization worklist.
- `bootstrap-target-layout`: Plan allocation handles, reclaim tickets, typed storage, and Drop calling
  shapes before MIR without choosing allocator policy.
- `bootstrap-mir`: Lower allocation, typed failure, storage initialization and rollback, and cleanup
  into the verified structured DAG.
- `bootstrap-evaluation`: Execute allocation identities, injected exhaustion, slot initialization,
  ownership transfer, and cleanup deterministically as the semantic oracle.
- `bootstrap-backend`: Realize the same compiler-planned allocation and cleanup contract in LLVM and
  direct Wasm without backend-specific semantic choices.
- `bootstrap-native-toolchain`: Extend the private platform shim only with the system allocate and
  infallible reclaim boundary required by `SystemAllocator`.
- `bootstrap-compiler-driver`: Gate the complete substrate through differential execution,
  fresh-process determinism, failure-ordinal sweeps, and no-artifact-on-invalid-program checks.
- `bootstrap-analysis-facade`: Expose the new canonical allocation and cleanup artifacts to clients
  without leaking evaluator or backend-private state.
- `bootstrap-syntax-inspector`: Add coordinated valid, invalid, exhausted, rollback, and early-drop
  presets to the existing unified `/labs` workbench.

## Impact

This changes the compiler frontend and every published intermediate representation, adds runtime and
backend support for owned allocation, extends evaluator state and cleanup traces, and adds a small
native/Wasm system-allocation boundary. It also expands the language and highlighting surfaces for
`unsafe`, Drop, and explicit `drop`. Existing non-allocating programs retain their semantics, but
artifact encodings and golden outputs gain new canonical node, layout, ownership, and trace forms.
