## ADDED Requirements

### Requirement: Alias names resolve to their erased target type

A type position that names a local alias, a selected alias import, or a namespace-qualified public
alias SHALL resolve to the alias target's canonical type. The lookup SHALL use the same immutable
module scope, collision rules, and visibility outcomes as nominal type resolution. An alias name
SHALL NOT resolve in value position. Resolution of a use SHALL NOT depend on whether the alias is
declared before or after the use, or in another module of the closure.

#### Scenario: Resolve a local alias

- **WHEN** a function contract names a unique local alias whose target is `i32 | Token`
- **THEN** the type lookup resolves the normalized union `i32 | Token`

#### Scenario: Resolve a qualified alias

- **WHEN** a module imports `net as Net` and a failure row names `Net.FetchError`
- **THEN** the type lookup resolves the public alias's erased target through the namespace alias

#### Scenario: Refuse an alias in value position

- **WHEN** an expression names a type alias as a value
- **THEN** analysis reports the ordinary unknown-value diagnostic and does not treat the alias as a constructor
