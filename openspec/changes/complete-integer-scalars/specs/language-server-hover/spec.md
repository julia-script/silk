## ADDED Requirements

### Requirement: Hover renders canonical integer types

Hover SHALL render lowercase integer spellings, `bool`, `()`, and `never`, never removed uppercase or backend lane names.

#### Scenario: Hover an inferred literal

- **WHEN** an unconstrained integer literal defaults successfully
- **THEN** hover reports `i32`

