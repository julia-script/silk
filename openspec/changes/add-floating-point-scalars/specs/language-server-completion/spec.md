## ADDED Requirements

### Requirement: Type completion offers float types

Type completion SHALL offer canonical `f32` and `f64` items derived from semantic type identities.

#### Scenario: Complete a float type

- **WHEN** completion is requested in a type position
- **THEN** `f32` and `f64` appear deterministically

