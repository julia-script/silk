## ADDED Requirements

### Requirement: Evaluation preserves exact Usize semantics

The evaluator SHALL represent `Usize` exactly across the selected 32- or 64-bit range and SHALL
evaluate checked arithmetic, unsigned comparisons, and traps without host-number precision loss.
Deterministic value and trace encodings SHALL use one canonical unsigned decimal form.

#### Scenario: Evaluate the native maximum exactly

- **WHEN** a native-target function evaluates the maximum `Usize` value without overflowing
- **THEN** the evaluator returns and encodes `18446744073709551615` exactly
