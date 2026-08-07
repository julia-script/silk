# bootstrap-owned-allocation Specification

## Purpose

Define the minimum target-aware owned-memory and deterministic-cleanup substrate from which Silk
standard-library collections can be implemented without named scopes or compiler-known allocators.

## Requirements

### Requirement: Layout is validated before allocation

`Layout` SHALL be a Copy value containing a target-sized byte count and validated power-of-two
alignment. Construction and repeated-element multiplication SHALL report invalid alignment and
representational overflow as ordinary validation data before any allocator or backend is entered.

#### Scenario: Reject overflowing repeated layout

- **WHEN** an element layout and runtime count exceed the selected target's `Usize` range
- **THEN** layout construction returns overflow data and performs no allocation

### Requirement: Allocation is a self-contained affine owner

The allocator service SHALL accept a validated `Layout` and return either typed `OutOfMemory` or one
affine `Allocation` carrying all authority needed for infallible release. The result MUST NOT borrow,
retain, or later rediscover the provider that created it.

#### Scenario: Drop after provider borrow ends

- **WHEN** an allocation escapes the call that borrowed `SystemAllocator`
- **THEN** the provider borrow ends, the allocation remains valid, and its eventual Drop releases through its captured reclaim authority exactly once

### Requirement: Raw typed storage is narrow and unsafe

The language SHALL expose the minimum unsafe typed-buffer and uninitialized-slot operations needed
to implement a collection. The compiler SHALL validate type/layout provenance, bounds ordering,
lexical slot access, and owner liveness while unsafe Silk code remains responsible for runtime
initializedness and aliasing invariants.

#### Scenario: Reject a slot after its buffer moves

- **WHEN** unsafe code retains a projected slot and attempts to move the backing raw buffer
- **THEN** ownership rejects the move before MIR or backend emission

### Requirement: Drop is synchronous infallible and deterministic

An affine nominal type MAY implement one restricted `Drop` hook. It SHALL be synchronous,
infallible, non-allocating, requirement-free, unable to move from `self`, and invoked before
automatic field cleanup. Live owners SHALL clean exactly once on fallthrough, return, `break`,
`continue`, typed failure, or explicit consuming `drop`; traps carry no cleanup guarantee.

#### Scenario: Roll back initialized elements on failure

- **WHEN** collection construction fails after initializing a prefix of move-only elements
- **THEN** Drop destroys exactly that prefix before the backing allocation releases and the original typed failure propagates unchanged

### Requirement: Vector is ordinary Silk code

`Vector<T>` SHALL be implemented in the Silk standard library from raw owned storage, length,
capacity, unsafe initialized-slot operations, and Drop. Growth SHALL allocate a replacement buffer,
move or copy exactly the initialized elements, commit only after success, and then drop the old
buffer; the compiler and backends MUST NOT contain collection-shaped allocation primitives.

#### Scenario: Preserve a vector after failed growth

- **WHEN** append requires growth and the replacement allocation fails
- **THEN** append fails with `OutOfMemory`, the original vector retains its prior elements and capacity, and no element or allocation is leaked or dropped twice

### Requirement: Allocator implementations receive no compiler privilege

The compiler SHALL dispatch allocators only through the general capability and conformance model.
Bootstrap SHALL provide `SystemAllocator`; an arena whose outputs depend on backing storage SHALL
remain unavailable until a general non-privileged validity mechanism or non-escaping API exists.

#### Scenario: Reject allocator-kind branching

- **WHEN** semantic, MIR, evaluator, or backend artifacts are inspected for a system allocation
- **THEN** they describe capability dispatch and reclaim authority without an allocator-kind tag or Arena-specific rule
