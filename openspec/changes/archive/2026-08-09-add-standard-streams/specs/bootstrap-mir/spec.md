## ADDED Requirements

### Requirement: MIR represents explicit byte writes

MIR SHALL represent an ordered effectful write over a destination and immutable byte view with typed failure. It MUST NOT encode file descriptors, JavaScript console calls, log metadata, or backend import names.

#### Scenario: Lower stdout write

- **WHEN** HIR writes bytes to stdout
- **THEN** MIR contains one target-neutral write operation after the byte view is available

