## ADDED Requirements

### Requirement: Scalar enum facts are analysis-facade queries

The analysis facade SHALL expose immutable scalar enum declaration identities, visibility,
representation states, ordered member identities and discriminants, member references, typed HIR,
ownership, match coverage, layout, MIR, evaluation, and emission provenance through the appropriate
frontend or realized snapshot. Tooling SHALL NOT reconstruct enum semantics from syntax, names, or
backing integers. Recovered and invalid enum states SHALL remain explicit and deterministic.

#### Scenario: Query a valid enum across phases

- **WHEN** a realized snapshot contains a used scalar enum
- **THEN** facade queries connect its declaration and member references to typed HIR, layout, verified MIR, evaluation, and emission facts

#### Scenario: Query an invalid enum without throwing

- **WHEN** an enum contains duplicate or overflowing discriminants
- **THEN** snapshot construction completes and facade queries expose the diagnostic-linked unavailable facts beside complete unrelated facts
