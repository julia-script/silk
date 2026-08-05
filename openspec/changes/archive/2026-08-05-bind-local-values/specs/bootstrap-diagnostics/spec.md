## ADDED Requirements

### Requirement: Ownership diagnostics join the unified model

The ownership phase SHALL publish its diagnostics as unified `Diagnostic` values identifying the
ownership phase as their originating phase, with stable `OWN`-prefixed codes, structured reason
data, and source-owned primary spans. Ownership diagnostics SHALL merge into the single
driver-side ordering after the semantic phase's rank and SHALL follow the same determinism,
never-print, and sentinel-provenance rules as every other phase.

#### Scenario: Ownership diagnostics carry their phase

- **WHEN** the ownership phase diagnoses a consumed binding used again
- **THEN** the diagnostic is a unified `Diagnostic` value identifying the ownership phase, and merging it with other phases' collections yields one deterministic sequence

#### Scenario: Ownership sentinels preserve provenance

- **WHEN** an ownership violation makes a function's verdict unsatisfied
- **THEN** the verdict retains the originating ownership diagnostic's identity so consumers can attach to the primary error rather than duplicate it
