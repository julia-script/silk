## ADDED Requirements

### Requirement: Operator markers are closed interface contract metadata

An interface operation MAY begin with `operator <token>` before its function contract. The marker
SHALL name exactly one supported eager prefix or infix token, SHALL agree with the operation's arity,
and SHALL be retained as semantic contract data. Services, free functions, control operators, and
operations with incompatible arity MUST NOT acquire operator eligibility.

#### Scenario: Retain one binary marker

- **WHEN** an interface declares `operator * fn multiply(left: Self, right: Scalar) -> Self`
- **THEN** its completed operation fact retains `*` as binary multiplication eligibility

#### Scenario: Reject control syntax as a marker

- **WHEN** an operation attempts to declare `operator &&`
- **THEN** declaration analysis reports the stable invalid-operator-contract diagnostic at the marker

#### Scenario: Reject a marker on a service

- **WHEN** a dependency-eligible service operation carries an operator marker
- **THEN** declaration analysis rejects it without changing any other service behavior
