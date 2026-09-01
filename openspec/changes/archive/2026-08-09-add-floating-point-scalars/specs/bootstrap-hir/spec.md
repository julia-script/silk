## ADDED Requirements

### Requirement: HIR carries canonical floats

HIR SHALL retain selected float width, correctly rounded constant bits, operation or conversion identity, evaluation order, and provenance without backend instructions or uncontrolled host coercion.

#### Scenario: Inspect an f32 literal

- **WHEN** a decimal literal is contextually typed `f32`
- **THEN** HIR encoding carries its canonical binary32 bits and source span
