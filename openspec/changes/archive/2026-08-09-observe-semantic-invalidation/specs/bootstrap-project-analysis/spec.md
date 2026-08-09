## ADDED Requirements

### Requirement: Project revisions expose semantic invalidation evidence

Each project analysis SHALL retain its current module semantic surfaces. Revising a prior accepted
project SHALL additionally expose one immutable semantic invalidation plan and phase observation
for the complete current union closure. Root views SHALL share those exact surfaces, observations,
and phase data by reference.

#### Scenario: Inspect semantic work after one edit

- **WHEN** one accepted edit produces a mix of reusable and recomputed semantic candidates
- **THEN** the project and every root view expose the same module classifications, reason counts, reuse count, and recomputation count

#### Scenario: Analyze without a prior project

- **WHEN** a project is analyzed without an adjacent accepted predecessor
- **THEN** every current module is classified as fresh semantic work and the project still publishes complete surfaces and totals

### Requirement: Invalidation observations do not reuse semantics prematurely

Until reusable semantic artifacts are introduced by a separate capability, project revision
analysis SHALL continue to construct a complete new declaration index, name resolution,
elaboration, ownership result, diagnostics, and tooling indexes for every current module. A
reusable classification in the invalidation plan SHALL describe expected future work only and MUST
NOT cause prior semantic objects to appear in the current frontend.

#### Scenario: Observe reusable candidates during global recomputation

- **WHEN** an adjacent revision classifies unchanged modules as reusable
- **THEN** their current semantic tables are newly constructed while the immutable invalidation evidence records that they were eligible for future reuse

### Requirement: Only the accepted predecessor governs semantic invalidation

Semantic invalidation planning SHALL compare a revision only with the prior completed project
supplied to project revision analysis. A stale or superseded LSP computation MUST NOT become the
predecessor of a later accepted revision.

#### Scenario: Supersede an in-flight revision

- **WHEN** revision N+1 finishes after N+2 has made it stale
- **THEN** a later revision compares against the last atomically committed project rather than N+1's uncommitted surfaces or observations
