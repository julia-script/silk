## ADDED Requirements

### Requirement: Evaluation executes scalar enums by declared member identity

Evaluation SHALL construct and copy enum members, compare equal enum values, expose discriminants
through `value`, and dispatch matches using the verified canonical member identity. It SHALL consume
the MIR representation plan and SHALL NOT admit arbitrary integers as enum values or independently
reinterpret structural-union tags.

#### Scenario: Evaluate enum construction and matching

- **WHEN** a program constructs one enum member, copies it, checks equality, reads `value`, and matches it
- **THEN** evaluation completes with the results implied by that member and its declared discriminant
