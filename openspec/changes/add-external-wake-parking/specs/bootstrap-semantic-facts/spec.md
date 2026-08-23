## ADDED Requirements

### Requirement: Sealed Wake values seed canonical local affinity

Every available sealed `Intrinsic.Wake` SHALL have canonical execution affinity `LocalExecution`.
That fact SHALL propagate through nominals, unions, arrays, callable and Effect environments,
Shared-held source state, and suspended frames by the existing `ExecutionAffinity` lattice. It
SHALL NOT publish an execution-instance identity, imply atomics, or grant thread transfer. An
ordinary source nominal named Wake MUST NOT receive the sealed seed.

#### Scenario: Propagate Wake affinity through source-owned state

- **WHEN** a Deferred-shaped or timer-shaped nominal stores Wake directly or behind Shared source state
- **THEN** the containing representation and every executable environment that captures it recursively remain `LocalExecution`

#### Scenario: Preserve local affinity across parking

- **WHEN** a Running Execution parks and transfers its sole Wake into source-owned registration state
- **THEN** the Wake, registration state, Dormant Execution, and suspended frame ownership remain in the canonical local execution domain without publishing an instance identity

#### Scenario: Keep transfer consumption deferred

- **WHEN** inspection observes a nominal containing Wake and every other component is unrestricted
- **THEN** it reports `LocalExecution` for a future transfer consumer without inventing transfer syntax, eligibility, or a diagnostic

#### Scenario: Deny a same-spelled nominal the sealed seed

- **WHEN** ordinary source declares a payload-free nominal named Wake
- **THEN** its affinity derives only from ordinary components and receives no intrinsic local-affinity privilege
