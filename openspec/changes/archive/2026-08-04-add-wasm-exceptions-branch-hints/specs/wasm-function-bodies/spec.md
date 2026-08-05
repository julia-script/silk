# wasm-function-bodies Delta

## ADDED Requirements

### Requirement: Exception instruction coverage
The system SHALL provide constructors, encoding, text rendering, and validation for `throw`
(popping the tag's parameter types), `throw_ref` (popping an `exnref`), and `try_table` — a
structured block whose catch clauses each target an enclosing label by relative depth. A
`catch` clause requires its label to accept the tag's parameters, `catch_ref` the parameters
plus `exnref`, `catch_all` nothing, and `catch_all_ref` exactly `exnref`. `exnref` SHALL be
usable wherever a reference type is.

#### Scenario: try_table catches into a matching label
- **WHEN** a body wraps a `throw` in a `try_table` whose `catch` clause targets a block
  accepting the tag's parameters
- **THEN** definition succeeds and both emitted representations agree

#### Scenario: Catch label arity mismatch rejected
- **WHEN** a `catch` clause targets a label that does not accept the tag's parameter types
- **THEN** definition fails with `WasmError`

#### Scenario: throw_ref requires an exnref
- **WHEN** a body applies `throw_ref` to an `i32`
- **THEN** definition fails with `WasmError`

### Requirement: Branch hints
The system SHALL accept an optional `likely` or `unlikely` hint on `br_if` and `if`
instructions, preserve it through both emitted representations, and reject hints anywhere the
specification does not define them.

#### Scenario: Hinted branch round-trips
- **WHEN** a body marks a `br_if` as likely and the module is emitted
- **THEN** the binary carries a branch-hint entry for that instruction's byte offset and the
  text carries the corresponding annotation
