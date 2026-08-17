## ADDED Requirements

### Requirement: Row-contract diagnostics are stable structured data

The generated diagnostic catalog SHALL contain distinct identities and structured payloads for row
kind mismatch, invalid singleton member, exact-access mismatch, checked absence, underconstrained
row computation, provider no-match, joint provider-selection conflict, provider ambiguity,
selected-row cardinality, conformance ambiguity, invalid conformance, cyclic substitution,
analysis-only availability, and non-concrete specialization.

Provider conflict and ambiguity payloads SHALL be span-free semantic data containing canonical
constraint/member keys and candidate sets. Primary and ordered secondary source origins SHALL live
in a separate diagnostic-location record and SHALL NOT affect diagnostic identity, payload equality,
or source/intrinsic parity. Precedence SHALL be syntax/kind, structural inference/underconstraint,
checked constraint failure, specialization non-concreteness, availability, then the existing
run-boundary `SEM0071` for an already concrete Effect.

#### Scenario: Report provider ambiguity deterministically

- **WHEN** unequal provider relation maps retain more than one common candidate
- **THEN** one diagnostic carries the common candidate list and every full relation candidate set, with selector/application primary and ordered relation secondary locations stored separately

#### Scenario: Keep equivalent call payloads equal

- **WHEN** equivalent source and intrinsic contracts fail at different source locations
- **THEN** their diagnostic identity and semantic payload are equal while their diagnostic-location records remain local

#### Scenario: Preserve run-boundary responsibility

- **WHEN** a concrete Effect reaches `run` with unsatisfied requirements
- **THEN** `SEM0071` remains the run-boundary diagnostic and is not reused for row inference or selection failures
