## ADDED Requirements

### Requirement: MIR lowering requires proven source returns

MIR lowering SHALL accept only executable HIR bodies carrying a complete semantic return proof.
The MIR verifier SHALL continue to reject malformed compiler-generated or hand-built returns as an
internal invariant, but a source return mismatch MUST NOT first surface from MIR or a backend.

#### Scenario: Stop a source mismatch before MIR

- **WHEN** an ordinary, effectful, generic, or conformance body violates its resolved return contract
- **THEN** no MIR function is lowered for that body and the source semantic diagnostic remains the primary failure
