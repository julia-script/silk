## ADDED Requirements

### Requirement: MIR receives only concrete row-contract instances

MIR lowering SHALL accept only branded specialized contracts whose failure and requirement rows are
concrete finite rows and whose used constraint evidence is concrete. MIR SHALL contain no row
parameters, symbolic members, `Without`, member-well-formedness obligations, assumed evidence,
provider candidate selection, or constraint entailment.

Concrete requirement binding SHALL lower from the exact selected stored member and provider match
already present in specialized HIR. Analysis-only intrinsics SHALL be rejected by the post-discovery
availability gate and SHALL never reach MIR. MIR verification and encoding SHALL remain
deterministic and backend-neutral.

#### Scenario: Lower an exact concrete requirement binding

- **WHEN** specialized HIR binds an exclusive provider to one concrete stored requirement
- **THEN** MIR consumes the branded selection evidence without reselecting or subtracting a row

#### Scenario: Reject symbolic row algebra at the MIR boundary

- **WHEN** a lowering request contains a residual row expression or assumed proof
- **THEN** MIR construction reports a compiler invariant violation rather than encoding symbolic state

#### Scenario: Exclude analysis-only catch from MIR

- **WHEN** a reachable selective catch depends on `AnalysisOnly(SEM0098)`
- **THEN** availability diagnoses it before any MIR operation is constructed
