## MODIFIED Requirements

### Requirement: Module conditions select declaration groups

Module scope SHALL accept `static if condition { declarations }` with optional `else { declarations }` or `else static if`. Nested groups SHALL publish selected declarations into their containing module namespace using ordinary declaration visibility. Every arm of a loaded source SHALL be parsed and lowered to authored HIR as a distinct conditional owner whose condition remains an unevaluated authored expression; local lowering SHALL NOT select an arm, evaluate a condition, or attach semantic diagnostics to an inactive arm. Inactive arms MUST NOT undergo ordinary name, type, Effect, ownership or backend analysis. A failed condition SHALL admit neither arm.

#### Scenario: Mutually exclusive surfaces

- **WHEN** mutually exclusive arms declare the same public function under two distinct profiles
- **THEN** each profile exposes exactly its selected declaration without a duplicate-name error

#### Scenario: Loaded inactive syntax

- **WHEN** an inactive arm of a loaded module has a syntax error and unresolved runtime names
- **THEN** its syntax error is reported and its ordinary semantic references are not checked

#### Scenario: Lower both arms before selection

- **WHEN** a loaded module spells `static if choose() { import a } else static if false { fn f() -> i32 { return missing() } }`
- **THEN** the authored module owns one conditional declaration whose then-group holds the import and whose else-branch is a nested conditional owner holding `f`
- **AND** `missing` is an authored identifier with no lexical binding and no semantic diagnostic
