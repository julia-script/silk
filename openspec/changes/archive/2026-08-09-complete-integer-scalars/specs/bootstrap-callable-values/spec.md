## ADDED Requirements

### Requirement: Integer actor callables use lowercase identities

Compiler-known integer actors SHALL use canonical lowercase source names such as `i32.add` and `u8.wrappingAdd`. Sections SHALL preserve the selected type and operation mode; uppercase actor names MUST NOT resolve as aliases.

#### Scenario: Construct a primitive section

- **WHEN** `i32.add(2)` appears where `fn(i32) -> i32` is required
- **THEN** it constructs the ordinary leading-argument section
