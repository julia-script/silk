## ADDED Requirements

### Requirement: Scalar enums are compiler-proved Copy values

Ownership analysis SHALL classify every valid scalar enum as sealed `Copy` and cleanup-free without
requiring a source conformance. Scalar enums SHALL NOT admit user `Copy` or `Drop` implementations or
conformances. Enum bindings SHALL follow the existing Copy read and explicit-move
rules, and cleanup plans SHALL contain no enum-specific release or drop operation.

#### Scenario: Plan a function holding enum values

- **WHEN** a function binds, copies, compares, and returns scalar enum values
- **THEN** ownership facts remain satisfied and the cleanup plan contains no release for those values
