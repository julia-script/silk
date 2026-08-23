## ADDED Requirements

### Requirement: Native and Wasm realize local shared ownership identically

Native LLVM and direct WebAssembly SHALL lower verified local-shared layout, initialization, clone,
callback access, conflict, and drop operations with the evaluator's observable transition order.
Each backend MAY choose private control-block field order, padding, reclaim representation, and
physical address, but MUST use the compiler-planned target layout and MUST NOT recognize `Shared`,
Deferred, Scheduler, or a ready inbox by spelling.

Both backends SHALL use non-atomic local state, compare the bounded strong count before any clone
mutation, keep strong-count and access state independent, form at most one callback borrow, leave
active access unchanged on conflict, restore access only after normal callback return, and clean `T`
exactly once before the final allocation release. Allocation exhaustion SHALL remain the ordinary
construction failure; clone, access, suspension, and return MUST NOT acquire an allocator channel.
Clone and access MUST NOT allocate or reallocate storage privately. Their lowering MUST NOT introduce
locks, atomics, scheduler machinery, garbage-collected backing, background work, or a runtime actor
selected by source spelling.
Fatal traps SHALL retain the existing no-unwind behavior, and strong cycles SHALL remain uncollected.

#### Scenario: Agree on successful access and cleanup

- **WHEN** one acyclic program constructs, clones, accesses, mutates, and drops an affine local shared value
- **THEN** native, Wasm, and evaluation agree on results, count transitions, access ordering, one payload cleanup, and one release

#### Scenario: Agree on every nested conflict combination

- **WHEN** shared and exclusive public access are nested in all four outer/inner combinations
- **THEN** both backends select the same conflict as evaluation before forming a second reference

#### Scenario: Trap before count mutation

- **WHEN** the target strong count is exhausted
- **THEN** each backend's clone path traps before its count store and returns no partial handle

#### Scenario: Preserve two-frame typed-failure cleanup

- **WHEN** a deeper frame drops one clone during typed-failure propagation and its caller later drops the final handle
- **THEN** every engine preserves the failure payload, performs one non-last decrement, and then cleans the value before release

#### Scenario: Distinguish physical representation from parity

- **WHEN** native and Wasm choose different private block layouts or reclaim metadata
- **THEN** source observes no layout lanes or address identity and all logical ownership outcomes remain equal

#### Scenario: Keep clone and access allocation-free

- **WHEN** either backend lowers and executes clone and callback access after one successful construction
- **THEN** structural and runtime evidence shows no further allocation or reallocation, lock, atomic, scheduler, collector, or background operation

#### Scenario: Leave a strong cycle allocated

- **WHEN** external handles to a local shared cycle are dropped on native and Wasm
- **THEN** neither backend synthesizes tracing, weak release, or cycle collection
