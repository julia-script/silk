## ADDED Requirements

### Requirement: Allocation facts separate access, ownership, and unsafe storage state

Semantic analysis SHALL publish canonical facts for validated `Layout`, selected allocator
capability, nominal provider, conformance witness, and role, typed `OutOfMemory`, affine
`Allocation`, private reclaim authority,
`RawBuffer<T>`, lexical `Slot<T>`, initialization operations, restricted Drop hooks, and explicit
drop. The provider loan SHALL end at the allocator call result; no returned fact may claim a
retained provider dependency. Invalid layouts, unavailable capabilities, unsafe-boundary omissions,
type/layout mismatches, slot escapes, and invalid Drop declarations SHALL retain unavailable facts
and stable diagnostics rather than fabricating usable storage.

#### Scenario: Resolve an independent allocation result

- **WHEN** an Effect allocates through `Allocator@Scratch` and returns the successful owner
- **THEN** facts expose the requirement and call-scoped exclusive access separately from the self-contained affine result and its possible `OutOfMemory`

#### Scenario: Keep a custom provider nominal

- **WHEN** a user-defined `TestAllocator` conformance satisfies an `Allocator` requirement
- **THEN** facts retain `TestAllocator` and its selected witness while the requirement remains keyed by `Allocator` and its role

#### Scenario: Reject safe raw storage access

- **WHEN** source invokes a RawBuffer or Slot operation that requires unsafe authority outside an `unsafe { ... }` boundary
- **THEN** semantic analysis records the missing-unsafe diagnostic and makes the operation result unavailable

#### Scenario: Reject a malformed Drop hook

- **WHEN** a declared Drop hook has a failure row or capability requirement
- **THEN** its semantic fact records the prohibited contract and no cleanup hook becomes available to ownership
