## ADDED Requirements

### Requirement: Struct construction authority is field-based

Source MAY construct a nominal struct from any module when every supplied or required initialized field is visible at the construction site. A private field SHALL preserve the type's construction boundary; visibility of another field or the type name SHALL NOT grant access to it.

#### Scenario: Construct from public fields

- **WHEN** an imported struct exposes all of its fields publicly
- **THEN** another module may construct it with named field initialization

#### Scenario: Preserve a private representation field

- **WHEN** one required field is private
- **THEN** external construction is rejected at that field and a public factory remains usable

### Requirement: Ordinary struct parameters infer from all supplied fields

Omitted ordinary generic arguments SHALL be inferred forward from all supplied field expressions using the same compatibility and conflict rules as function calls. Explicit type arguments SHALL form a prefix, and ambiguity or disagreement SHALL produce deterministic diagnostics.

#### Scenario: Infer one parameter from multiple fields

- **WHEN** `Pair<T>` is constructed with two fields that both resolve to `i32`
- **THEN** the constructed type is `Pair<i32>` without an explicit argument

#### Scenario: Diagnose conflicting fields

- **WHEN** two fields constrain the same omitted parameter to incompatible types
- **THEN** analysis reports both field constraints and does not choose by source order
