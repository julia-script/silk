## ADDED Requirements

### Requirement: Project revisions structurally share reusable module semantics

After the current closure's declaration facts, resolution, surfaces, and invalidation plan are
complete, project revision analysis SHALL retain the exact prior semantic artifact for every module
classified reusable and SHALL recompute semantic artifacts only for the invalidated modules. The
result SHALL still be one complete immutable project with one deterministic merged diagnostic
sequence and structurally shared root views.

#### Scenario: Revise one unrelated root

- **WHEN** one root changes while another root and its dependency region remain semantically reusable
- **THEN** the new project shares the unaffected modules' semantic artifacts and recomputes only the invalidated region

#### Scenario: Analyze a fresh project

- **WHEN** project analysis has no accepted predecessor
- **THEN** every current module receives a newly computed semantic artifact and reuse totals are zero

### Requirement: Global indexes remain current during module semantic reuse

Every project revision SHALL construct declaration collection, declaration completion, name
resolution, semantic surfaces, merged diagnostics, and project tooling indexes for the complete
current closure until a later capability makes those artifacts composable. Reusing a module
semantic artifact MUST NOT substitute a predecessor project index, resolution, tooling index, or
merged diagnostic sequence.

#### Scenario: Reuse semantics inside a new project

- **WHEN** an adjacent revision shares one or more module semantic artifacts
- **THEN** every root view combines those artifacts with the new project's current declaration, resolution, diagnostic, and tooling facts

#### Scenario: Recover from an invalid edit

- **WHEN** a changed module produces unavailable or erroneous current facts
- **THEN** the current project exposes those facts and diagnostics without falling back to its prior valid semantic artifact

## REMOVED Requirements

### Requirement: Invalidation observations do not reuse semantics prematurely

**Reason**: The separately specified module-semantic artifact capability now defines and implements
the safe ownership and invalidation boundary required for actual reuse.

**Migration**: Consumers SHALL use the current project's module semantic artifacts and reuse phase
observations; declaration, resolution, diagnostics, and tooling remain current-project facts.
