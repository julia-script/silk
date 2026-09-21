# explicit-mir-lowering Specification

## ADDED Requirements

### Requirement: MIR lowering has explicit artifact inputs

`Mir.lower` SHALL accept concrete instances, runtime layout, declaration facts, opaque realization
facts, current presentation, optional profile, admission, normalization, and audit policy without a
frontend or source resolver.

#### Scenario: Admitted lowering

- **WHEN** admitted valid artifacts are supplied
- **THEN** the operation produces finalized MIR and its diagnostics

#### Scenario: Rejected admission

- **WHEN** admission is rejected with diagnostics
- **THEN** no lowering executes and the result contains no program

### Requirement: Stage policy is visible

Normalization and audit behavior SHALL be selected by closed input values and SHALL NOT be inferred
from caller identity or hidden coordinator state.

#### Scenario: Preserve provisional shape

- **WHEN** normalization is `Preserve`
- **THEN** the operation returns the unnormalized program after mandatory ownership/native checks

#### Scenario: Foreign planning audit

- **WHEN** audit is `ForeignPlanning` and the program violates planning constraints
- **THEN** diagnostics are returned and the program is withheld
