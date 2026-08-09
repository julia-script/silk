## MODIFIED Requirements

### Requirement: Global indexes remain current during module semantic reuse

Every project revision SHALL construct declaration collection, declaration completion, name
resolution, semantic surfaces, and merged diagnostics for the complete current closure. Project
tooling indexes SHALL be composed from current module tooling artifacts, structurally sharing only
artifacts whose exact semantic inputs are shared. Reusing module semantics or tooling MUST NOT
substitute a predecessor project declaration index, resolution, or merged diagnostic sequence.

#### Scenario: Reuse semantics and tooling inside a new project

- **WHEN** an adjacent revision shares one or more module semantic and tooling artifacts
- **THEN** every root view combines those artifacts with the new project's current declaration, resolution, diagnostic, and composed tooling facts

#### Scenario: Recover from an invalid edit

- **WHEN** a changed module produces unavailable or erroneous current facts
- **THEN** the current project exposes those facts and diagnostics with newly computed tooling for that module rather than falling back to prior valid facts
