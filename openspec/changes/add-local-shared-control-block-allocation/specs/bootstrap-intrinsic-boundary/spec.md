## ADDED Requirements

### Requirement: Two sealed primitives fund local shared construction

The sealed `Intrinsic` namespace SHALL expose exactly the construction operations
`sharedLayout<T>() -> Layout` and unsafe
`sharedFromAllocation<T>(allocation: Allocation, value: T) -> SharedCore<T>` for local shared
ownership. `sharedLayout` SHALL be pure, allocation-free, target-aware, and specialized by concrete
`T`. `sharedFromAllocation` SHALL consume both arguments, accept only the exact planned layout, and
publish one initialized core without a failure or requirement channel.

Both operations SHALL declare normalized availability for evaluation, every supported native target,
and direct WebAssembly. A `sharedLayout<T>` specialization whose complete control block cannot be
represented by the selected target SHALL remain unavailable before MIR and execution, retaining a
stable diagnostic at the intrinsic call; it MUST NOT return a partial `Layout`, runtime validation
member, allocation failure, or trap.

Neither primitive MAY recognize an allocator implementation, allocate storage, expose a raw shared
address or reclaim operation, choose source conflict policy, or recognize a standard-library actor
by spelling. No ordinary declaration outside `Intrinsic` may obtain these contracts from its name.

#### Scenario: Audit the construction inventory

- **WHEN** the sealed intrinsic inventory is encoded for an available target
- **THEN** it contains the two generic construction contracts with their exact safety, access, ownership, failure, and requirement metadata

#### Scenario: Keep layout planning allocation-free

- **WHEN** source evaluates `sharedLayout<T>()`
- **THEN** it receives validated layout data without allocator access, storage acquisition, or a new cleanup obligation

#### Scenario: Reject an unrepresentable control-block layout

- **WHEN** header addition, alignment rounding, or payload placement for concrete `T` exceeds the selected target's representable layout
- **THEN** target layout marks the specialization unavailable with a stable diagnostic at the `sharedLayout<T>` call before MIR, allocation, or execution

#### Scenario: Require unsafe construction syntax

- **WHEN** source calls `sharedFromAllocation<T>` without an explicit unsafe boundary
- **THEN** analysis rejects the call at that source boundary before consuming the allocation or value

#### Scenario: Ignore same-spelled ordinary operations

- **WHEN** ordinary source declares operations named `sharedLayout` or `sharedFromAllocation` outside the sealed `Intrinsic` namespace
- **THEN** both declarations retain ordinary source contracts and receive no intrinsic identity, safety rule, target availability, or lowering behavior from spelling

#### Scenario: Keep policy actors ordinary

- **WHEN** compiler catalogs and phase dispatch are inspected after construction support is added
- **THEN** no entry or branch names `Shared`, `Allocator`, `OutOfMemoryError`, Deferred, Scheduler, or a ready inbox as a privileged actor
