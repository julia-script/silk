## ADDED Requirements

### Requirement: Backends emit conservative floating operations

LLVM SHALL emit ordinary float operations without implicit fast-math flags; direct WebAssembly SHALL emit corresponding `f32`/`f64` instructions. Both SHALL realize MIR comparison, classification, total order, reinterpretation, and conversion semantics consistently.

#### Scenario: Emit f64 arithmetic

- **WHEN** accepted `f64` arithmetic lowers
- **THEN** generated artifacts contain no reassociation, no-NaN, no-infinity, or equivalent promises
