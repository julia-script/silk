## ADDED Requirements

### Requirement: Exclusive storage retains lifetime and access authority

Stored exclusive references SHALL remain affine through fields, generic wrappers, arrays, extraction, restoration and ordinary results. Their outer access lifetime is covariant and their payload type invariant. Nominal variance SHALL derive from declared storage over a finite lattice; opaque unsafe storage defaults invariant. Dependent Effect outcomes and suspension with partial owners remain rejected.

#### Scenario: Shared descendants retain exclusivity

- **WHEN** a shared child of an exclusive stored view is copied and one child ends
- **THEN** conflicting parent access remains rejected until every dependent ends; parent access is accepted afterward
