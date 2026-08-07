## ADDED Requirements

### Requirement: Flow and failure syntax is lossless and locally recoverable

The syntax layer SHALL preserve `flow fn`, `!` failure rows, prefix `run`, `fail move`, and an
explicit catch type argument on a pipeline target with every token and source span. Recovery from a
missing function keyword, row member, run operand, move keyword, or failure operand SHALL stay
within the damaged declaration or statement.

#### Scenario: Parse the complete surface

- **WHEN** source declares a flow with two failure members, constructs it, runs it, and originates one failure
- **THEN** the syntax tree retains distinct flow declaration, failure row, run expression, and fail statement nodes

#### Scenario: Recover a missing row member

- **WHEN** `!` is followed by a body delimiter
- **THEN** one missing type is retained and the following body remains a separate block

#### Scenario: Parse pipelined exact recovery

- **WHEN** a flow recipe is piped into `Flow.catch<E>(handler)`
- **THEN** the pipeline target retains the explicit `E` and the handler argument losslessly
