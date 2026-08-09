## ADDED Requirements

### Requirement: Evaluation is bit-aware for floats

Evaluation SHALL store explicit float width and IEEE bits, round after every `f32` operation, preserve signed zero, canonicalize arithmetic NaNs where payload is unspecified, and implement classification, total order, reinterpretation, and conversions deterministically.

#### Scenario: Round an f32 operation

- **WHEN** an `f32` arithmetic result needs binary32 rounding
- **THEN** evaluation rounds once at that operation and matches both backends

#### Scenario: Preserve fromBits-toBits

- **WHEN** a float is created from same-width integer bits and reinterpreted back
- **THEN** evaluation returns the original bits exactly

