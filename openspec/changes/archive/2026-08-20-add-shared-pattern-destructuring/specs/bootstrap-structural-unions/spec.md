## ADDED Requirements

### Requirement: Pattern selection uses exact normalized union members

Pattern analysis SHALL select ordinary union members by canonical normalized type identity.
Nominal values, scalars, arrays, strings, and finite represented executable members MAY be exact
whole-member selectors when they are valid members of the scrutinee. Selection SHALL NOT invent a
separate nominal-only membership relation or expose numeric runtime tags.

#### Scenario: Select a scalar member

- **WHEN** an `i32 | string` value is matched by an `i32 number` pattern
- **THEN** the selected binding has exact type `i32` and coverage removes that canonical member

#### Scenario: Reject a foreign exact member

- **WHEN** a pattern selects `bool` from `i32 | string`
- **THEN** analysis identifies `bool` as absent from the normalized scrutinee members
