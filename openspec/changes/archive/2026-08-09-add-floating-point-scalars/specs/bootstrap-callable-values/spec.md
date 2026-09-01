## ADDED Requirements

### Requirement: Floating actor operations are callable values

`f32` and `f64` actor operations SHALL support ordinary named references and leading-argument sections while preserving width and operation identity.

#### Scenario: Construct an f64 section

- **WHEN** `f64.add(2.0)` appears where `fn(f64) -> f64` is expected
- **THEN** it constructs a width-preserving callable section
