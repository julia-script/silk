## ADDED Requirements

### Requirement: Usize has target-aware differential acceptance

The compiler acceptance surface SHALL compare evaluator, native, and Wasm results for `Usize`
programs whose values fit 32 bits, compare evaluator and native results above 32 bits, and require
Wasm target rejection for out-of-range literals before emission. Fresh-process runs SHALL preserve
identical facts, layouts, MIR, textual artifacts, and binary artifacts for the same target.

#### Scenario: Compare the shared range

- **WHEN** a canonical fixture uses checked `Usize` arithmetic entirely within the 32-bit range
- **THEN** evaluator, native execution, and Wasm execution return the same unsigned value

#### Scenario: Compare the native-only range

- **WHEN** a canonical native fixture computes a valid value above `2^32 - 1`
- **THEN** evaluator and native execution agree exactly while the Wasm-targeted counterpart is rejected before MIR
