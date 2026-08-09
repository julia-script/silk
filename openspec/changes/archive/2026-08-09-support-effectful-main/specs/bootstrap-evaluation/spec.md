## MODIFIED Requirements

### Requirement: First bootstrap entry point

Bootstrap evaluation SHALL execute the snapshot's lowered MIR program from the entry that instance
discovery resolved. An ordinary `main() -> I32` SHALL retain its exact completed value. An effectful
`main() -> Unit ! E` SHALL be constructed and run once by the lowered entry adapter, producing
either completed status `0` or deterministic unhandled-failure termination data retaining the
normalized failure tag and canonical identity. When discovery reports an unavailable entry,
evaluation SHALL return a closed `Blocked` outcome carrying that explicit entry reason rather than
throw, fail, or choose a declaration.

#### Scenario: Select one ordinary main

- **WHEN** discovery resolves exactly one zero-parameter ordinary `main` declaring `I32`
- **THEN** evaluation enters that instance's lowered function and preserves its exact result

#### Scenario: Select one effectful main

- **WHEN** discovery resolves exactly one zero-parameter effectful `main` succeeding with `Unit`
- **THEN** evaluation enters the generated adapter and runs the entry Effect exactly once

#### Scenario: Retain an unhandled entry failure

- **WHEN** effectful `main` fails with a reportable failure
- **THEN** evaluation returns deterministic termination data naming its normalized tag and canonical identity

#### Scenario: Block a missing main

- **WHEN** no top-level declaration is named `main`
- **THEN** evaluation is blocked with the missing-entry reason and does not select another function

#### Scenario: Block ambiguous main declarations

- **WHEN** multiple top-level declarations are named `main`
- **THEN** evaluation is blocked with the ambiguous-entry reason and does not choose the first

#### Scenario: Block a parameterized main

- **WHEN** the unique `main` declaration has one or more parameters
- **THEN** evaluation is blocked with the parameterized-entry reason
