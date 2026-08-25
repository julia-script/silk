## ADDED Requirements

### Requirement: Scalar enum declarations have one canonical layout

The formatter SHALL preserve comments and source meaning while rendering optional visibility,
`enum`, an optional representation without interior padding, the enum name, braces, and ordered
members canonically. A multiline enum SHALL place one member per line, retain an explicit
` = <signed-decimal-literal>` when present, use canonical trailing commas, and remain idempotent.
Formatting SHALL NOT infer, insert, remove, or renumber discriminants or insert an omitted `u8`
representation.

#### Scenario: Format a represented enum

- **WHEN** a valid `enum(u8)` declaration contains explicit and implicit members with irregular whitespace
- **THEN** formatting produces canonical spacing and indentation while preserving the representation and every explicit discriminant

#### Scenario: Keep default representation omitted

- **WHEN** a default enum omits its representation clause
- **THEN** formatting remains idempotent without inserting `(u8)`
