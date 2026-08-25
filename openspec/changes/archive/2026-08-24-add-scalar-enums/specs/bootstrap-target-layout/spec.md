## ADDED Requirements

### Requirement: Scalar enum layouts reuse validated representation layouts

Target layout planning SHALL give every valid scalar enum the exact size, alignment, and calling
shape of its declared fixed-width integer representation while retaining the enum's canonical nominal
identity in layout facts. Planning SHALL add no hidden metadata and SHALL leave only the dependent
enum layout unavailable when representation or discriminant validation fails.

#### Scenario: Plan default and explicit layouts

- **WHEN** one enum defaults to `u8` and another explicitly selects `i32`
- **THEN** their physical layouts exactly match `u8` and `i32` respectively on every supported target

#### Scenario: Isolate an invalid enum layout

- **WHEN** one enum has an unsupported representation beside a valid enum
- **THEN** only the invalid enum layout is unavailable and the valid enum layout remains complete
