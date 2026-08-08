## ADDED Requirements

### Requirement: The private platform boundary supports system allocation

The compiler-versioned native runtime shim SHALL expose only the aligned system acquisition and
infallible release operations needed by the standard-library `SystemAllocator` conformance. A valid
layout SHALL produce either an opaque successful block identity or an allocation-free exhaustion
status. The boundary SHALL preserve requested size and alignment for release, support valid
zero-sized and over-aligned layouts, and expose no public `free`, resize, zero-fill, allocator-kind,
or stable external ABI promise.

#### Scenario: Translate native exhaustion

- **WHEN** the private aligned acquisition boundary cannot satisfy a valid layout
- **THEN** native execution returns the status used for typed `OutOfMemory` and creates no releasable block

#### Scenario: Release an over-aligned block

- **WHEN** native execution drops a successful over-aligned Allocation
- **THEN** its captured reclaim ticket invokes the matching private release exactly once with no ambient provider lookup
