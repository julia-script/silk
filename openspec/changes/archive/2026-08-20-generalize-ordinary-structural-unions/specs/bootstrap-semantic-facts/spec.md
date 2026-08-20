## MODIFIED Requirements

### Requirement: Union facts expose canonical and source member structure

Semantic analysis SHALL retain each source-written union member and separator with its resolution,
provenance, and causal availability while publishing one normalized type outcome containing the
canonical ordered ordinary member set. Each admitted member SHALL retain the exact evidence needed
to distinguish its public type and, for represented executable values, its compiler-private finite
representation. Failed members SHALL remain queryable and MUST make the dependent union outcome
unavailable without erasing independent resolved members.

#### Scenario: Analyze an equivalent duplicate union

- **WHEN** source spells `i32 | Token | i32`
- **THEN** facts retain three source members while the available semantic type contains canonical `i32` and `Token` exactly once

#### Scenario: Retain represented executable evidence

- **WHEN** an exact callable or opaque Effect value enters a union
- **THEN** its conversion fact identifies the exact represented source and canonical target member without exposing a public runtime tag

#### Scenario: Retain one unresolved member

- **WHEN** one member of `Token | Missing | i32` cannot resolve
- **THEN** the `Token` and `i32` member facts remain available and the union outcome names the missing member's cause
