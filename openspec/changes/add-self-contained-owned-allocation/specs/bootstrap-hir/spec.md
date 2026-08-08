## ADDED Requirements

### Requirement: HIR retains allocation and cleanup semantics without policy

HIR SHALL represent validated and repeated layout formation, general allocator capability dispatch,
typed allocation success or `OutOfMemory`, self-contained allocation ownership, unsafe RawBuffer and
Slot operations, initialization transitions, restricted Drop declarations and calls, explicit drop,
and automatic cleanup with canonical types and source provenance. HIR MUST NOT encode allocator
implementation kinds, provider-dependent result lifetimes, named lifetime scopes, dynamic finalizer
registries, host addresses, or backend heap policy.

#### Scenario: Elaborate a raw construction guard

- **WHEN** unsafe source allocates repeated storage and initializes a runtime prefix under a Drop guard
- **THEN** HIR retains one typed buffer identity, checked slot projections, prefix updates, hook identity, and typed failure branch in source order

#### Scenario: Keep SystemAllocator ordinary

- **WHEN** a call resolves through a `SystemAllocator` conformance witness
- **THEN** HIR records the general capability dispatch and concrete witness identity without a system-allocator operation tag
