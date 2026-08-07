## ADDED Requirements

### Requirement: MIR represents typed outcomes in the structured DAG

MIR SHALL represent flow calls, success returns, nominal failure returns, propagation, and catch
dispatch as explicit target-aware structured operations and outcomes. Verification SHALL reject row,
tag, payload, calling-shape, ownership, cleanup, or target inconsistencies before execution. The
compiler representation SHALL remain a DAG.

#### Scenario: Verify propagation cleanup

- **WHEN** a flow call may fail after earlier locals became live
- **THEN** MIR contains distinct success and failure continuations with cleanup before forwarding the failure

#### Scenario: Reject a forged failure tag

- **WHEN** malformed MIR associates a payload with another nominal member's tag
- **THEN** verification rejects it before evaluator or backend execution
