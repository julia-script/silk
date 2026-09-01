## ADDED Requirements

### Requirement: MIR lowers composed Effect recipes completely

MIR lowering SHALL recursively realize every semantically valid Effect recipe nested beneath
`run`, including transformations whose protected recipe is provisioned, recovered, retried, or
acquired. Direct and stored forms MUST select complete deterministic runner identities, callable
environments, provider arguments, failure mappings, loan boundaries, and cleanup regions. Valid
source MUST NOT lower to an unavailable-transform trap, an unpublished region, or a compiler
implementation exception.

#### Scenario: Lower map around provision

- **WHEN** a run subject is an `Effect.map` whose protected Effect is a service-provision recipe
- **THEN** MIR contains a complete execution path for the protected runner, provider, and mapper with no unavailable region

#### Scenario: Lower provision around transformation

- **WHEN** a run subject provides a requirement after one or more transformations preserve it
- **THEN** MIR passes the provider through the transformed execution and closes its loan at the composed run boundary

#### Scenario: Lower a stored composed recipe

- **WHEN** the same recipe tree is stored in a binding before `run`
- **THEN** MIR preserves its eager construction facts and emits behavior equivalent to the direct recipe tree

#### Scenario: Reject an invalid composition before MIR

- **WHEN** types, failures, requirements, callable access, or ownership make a pipeline invalid
- **THEN** semantic analysis reports the relevant source diagnostic and MIR emission remains unavailable without a fallback trap
