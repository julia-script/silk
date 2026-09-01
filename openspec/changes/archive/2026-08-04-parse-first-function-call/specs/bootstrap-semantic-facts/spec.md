## ADDED Requirements

### Requirement: Unresolved call expression fact

For every concrete call return expression, semantic analysis SHALL publish a call fact containing a
present or unavailable callee-name state and exact call/callee syntax provenance. A present callee
SHALL remain explicitly `Unresolved` in this syntax change, SHALL produce no unknown-name semantic
diagnostic, and SHALL make the caller's return compatibility `Unavailable`. Missing or damaged call
syntax SHALL remain unavailable without duplicating parser diagnostics.

#### Scenario: Preserve a present unresolved call

- **WHEN** `main` returns the concrete call `answer()`
- **THEN** its returned-expression fact preserves the spelling and provenance of `answer`, marks the reference unresolved, reports unavailable compatibility, and emits no semantic diagnostic for the callee

#### Scenario: Preserve a damaged call without semantic duplication

- **WHEN** parser recovery inserts the call's callee or a parenthesis
- **THEN** the call fact preserves unavailable syntax, return compatibility is unavailable, and parser diagnostics remain the only diagnostics for the missing syntax

#### Scenario: Keep integer facts available beside calls

- **WHEN** one function returns `42` and another returns `answer()`
- **THEN** the first function retains its exact available integer and compatibility facts while the call fact remains unresolved
