## ADDED Requirements

### Requirement: MIR represents target-selected Usize values in the DAG

MIR SHALL represent `Usize` literals and operations with the selected compiler-owned unsigned word
lane. Verification SHALL reject out-of-range literals, mismatched operand widths or types, signed
comparison/division semantics, and arithmetic results lacking the required overflow or underflow
trap behavior. The structured control representation SHALL remain a DAG.

#### Scenario: Reject a mismatched word lane

- **WHEN** malformed native MIR assigns a 32-bit lane to `Usize`
- **THEN** verification rejects it before evaluation or backend emission
