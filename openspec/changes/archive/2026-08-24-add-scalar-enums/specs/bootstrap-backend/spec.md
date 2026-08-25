## ADDED Requirements

### Requirement: Backends realize verified scalar enums without new runtime metadata

Wasm and native backends SHALL lower scalar enum values, parameters, results, equality, `value`, and
match dispatch through the exact integer lane selected by the MIR representation plan. Backends SHALL
NOT choose a representation, add metadata, synthesize undeclared enum inhabitants, or treat
structural-union tags as the scalar-enum public value. Equivalent verified MIR SHALL produce
observably equivalent results on every supported engine.

#### Scenario: Lower a signed enum across engines

- **WHEN** verified MIR passes an `enum(i8)` member with discriminant `-1` through a function and returns its `value`
- **THEN** Wasm and native execution both produce `-1` through the canonical `i8` calling shape

#### Scenario: Lower enum match dispatch

- **WHEN** verified MIR matches a scalar enum exhaustively
- **THEN** each backend dispatches only among declared member decisions and selects the same arm as evaluation
