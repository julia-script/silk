# semantic-evaluation-queries Specification

## ADDED Requirements

### Requirement: Evaluation identity is value-sensitive

The compiler SHALL key evaluation by the target/profile and source identities plus declaration,
type arguments, evidence, contract row, and static argument values.

#### Scenario: Different static values

- **WHEN** two applications differ only in a static argument value
- **THEN** they execute under distinct evaluation keys

#### Scenario: Equivalent repeated application

- **WHEN** an equivalent application is demanded again
- **THEN** the existing evaluation store may reuse its outcome while charging its recorded cost

### Requirement: Failure and interruption do not poison evaluation

Pending recursion SHALL report a structured cycle, deterministic limit failures SHALL follow the
existing root/nested cache policy, and defects SHALL remove pending reservations.

#### Scenario: Provider defect and retry

- **WHEN** evaluation defects before completing
- **THEN** its pending entry is removed and a later demand may execute again
