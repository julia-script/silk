## ADDED Requirements

### Requirement: Nominal union ownership follows nominal struct rules

A union value SHALL be affine by default. `Copy` and `Drop` implementations, generic Copy bounds,
moves, borrows, writes, partial-move rejection, and implementation admissibility SHALL follow the
same rules as nominal structs across every variant payload. The compiler MUST NOT infer Copy merely
because all currently reachable payload fields are Copy.

#### Scenario: Require an explicit Copy implementation

- **WHEN** every field of every variant is Copy but the union declares no valid `impl Copy`
- **THEN** reading the union as a whole consumes it under ordinary affine ownership

#### Scenario: Validate Copy across every variant

- **WHEN** a union requests `Copy` and one variant contains an affine field under the declared bounds
- **THEN** conformance is rejected at that field even when another variant is unit

### Requirement: Cleanup follows exactly one active variant

Owned union cleanup SHALL run the union's admitted nominal cleanup behavior and recursively clean
exactly the initialized fields of the active variant once. Variant selection, structural-union
injection, moves, borrows, typed-failure transfer, ordinary scope exits, and generic specialization
MUST preserve that single active obligation. Fatal traps SHALL retain the existing no-unwind rule.

#### Scenario: Clean one selected payload

- **WHEN** a union holding a droppable field in one variant leaves scope
- **THEN** every engine runs the union-level and active-field cleanup prescribed by ordinary struct ordering without touching inactive variant storage

#### Scenario: Consume one field variant through matching

- **WHEN** a moved match extracts one payload field and omits another with `..`
- **THEN** ownership transfers the extracted field once and cleans only the selected variant's omitted fields
