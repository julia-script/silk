## ADDED Requirements

### Requirement: MIR receives only concrete row-contract instances

MIR lowering SHALL accept only branded specialized contracts whose failure and requirement rows are
concrete finite rows and whose used constraint evidence is concrete. MIR SHALL contain no row
parameters, symbolic members, `Without`, member-well-formedness obligations, assumed evidence,
provider candidate selection, or constraint entailment.

Concrete requirement binding SHALL lower from the exact selected stored member and provider match
already present in specialized HIR. Selective failure handling SHALL lower from the concrete
protected row, selected nominal member, handler contract, and residual result row already present in
specialized HIR. MIR verification and encoding SHALL remain deterministic and backend-neutral.

#### Scenario: Lower an exact concrete requirement binding

- **WHEN** specialized HIR binds an exclusive provider to one concrete stored requirement
- **THEN** MIR consumes the branded selection evidence without reselecting or subtracting a row

#### Scenario: Reject symbolic row algebra at the MIR boundary

- **WHEN** a lowering request contains a residual row expression or assumed proof
- **THEN** MIR construction reports a compiler invariant violation rather than encoding symbolic state

#### Scenario: Lower executable selective catch

- **WHEN** a reachable selective catch has a concrete protected row and selected nominal member
- **THEN** MIR runs the protected Effect once, bypasses the handler on success, invokes it for the selected failure tag, and remaps every other failure into the residual row
