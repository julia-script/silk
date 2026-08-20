## ADDED Requirements

### Requirement: Copy is a sealed zero-operation conformance

`Copy` SHALL be a compiler-sealed interface with no user-definable operations. An implementation
MAY be declared only in the empty form and SHALL publish evidence only after the compiler proves
every stored field Copy, proves the complete type cleanup-free, and finds no `Drop` implementation,
cycle, or conflicting evidence.

#### Scenario: Reject a Copy operation body

- **WHEN** an implementation of `Copy` declares or maps an operation
- **THEN** conformance validation rejects it rather than treating duplication as user code

#### Scenario: Reject Copy and Drop together

- **WHEN** one provider attempts to implement both `Copy` and `Drop`
- **THEN** the Copy implementation is invalid and no Copy witness is published
