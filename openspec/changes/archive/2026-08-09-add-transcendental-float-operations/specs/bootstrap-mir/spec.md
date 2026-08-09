## ADDED Requirements

### Requirement: MIR represents floating transcendental operations

MIR SHALL carry explicit width-specific `Sin` and `Cos` operations with one floating operand,
same-width result, and source provenance. Verification SHALL reject integer operands, width
mismatches, and unknown transcendental operation names; deterministic encoding SHALL retain the
operation and width.

#### Scenario: Verify f64 sine

- **WHEN** a valid `f64.sin` expression lowers to MIR
- **THEN** verification accepts one `Sin` operation whose operand and result are both `f64`

#### Scenario: Reject a mismatched cosine

- **WHEN** malformed MIR assigns an `f32` cosine result to an `f64` destination
- **THEN** verification rejects the function before evaluation or backend emission
