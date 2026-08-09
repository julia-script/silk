## ADDED Requirements

### Requirement: Hover renders exact float width

Hover SHALL render `f32` or `f64` from semantic facts and never substitute a backend lane or generic number label.

#### Scenario: Hover a default float literal

- **WHEN** an unconstrained float literal is accepted
- **THEN** hover reports `f64`

