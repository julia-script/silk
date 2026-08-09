# bootstrap-project-analysis Specification

## Purpose
Defines coherent multi-root frontend analysis that computes shared compiler facts once per project
revision and exposes immutable root views without duplicating module work.
## Requirements
### Requirement: One project revision analyzes the union module closure once

Project frontend analysis SHALL accept one or more canonical root sources, resolve the union of
their reachable module closures, and process each canonical module at most once for that project
revision. Root supply order SHALL NOT change module order, diagnostics, facts, or observations.

#### Scenario: Roots share a dependency

- **WHEN** two project roots both reach the same imported module
- **THEN** the project analysis loads, parses, indexes, elaborates, and ownership-checks that module once while retaining both roots

#### Scenario: Repeat roots in another order

- **WHEN** equivalent root sources are supplied in a different order
- **THEN** the project analysis exposes identically ordered modules, facts, diagnostics, and deterministic observation counts

### Requirement: Root views share one immutable project revision

Project frontend analysis SHALL derive an immutable view for each requested root. Every view SHALL
identify its own root while structurally sharing the project revision's source, syntax, declaration,
resolution, elaboration, ownership, tooling-index, diagnostic, and phase-observation facts. Deriving
or querying one view SHALL NOT mutate another view or execute compiler phases again.

#### Scenario: Compare two root views

- **WHEN** two open roots receive views from one completed project analysis
- **THEN** each view identifies the requested root and references the same immutable project facts and phase observations

#### Scenario: Query views independently

- **WHEN** consumers issue module-qualified frontend queries through different root views
- **THEN** both receive coherent answers from the same project revision without additional analysis work

### Requirement: Project analysis remains frontend-only

Project frontend analysis and root-view derivation SHALL NOT execute instance discovery, target
selection, layout planning, MIR lowering, evaluation, or code generation.

#### Scenario: Observe a completed project revision

- **WHEN** a multi-root project frontend analysis completes
- **THEN** its observations contain one frontend phase sequence for the union closure and no runtime realization phase

### Requirement: Project analysis revises an accepted immutable project

Project frontend analysis SHALL accept an optional prior completed project analysis as a reuse
basis. The resulting project SHALL be a new immutable coherent analysis over the current roots and
resolved closure; the prior project SHALL remain unchanged.

#### Scenario: Revise one edited root

- **WHEN** one root changes and a prior completed multi-root project is supplied
- **THEN** analysis returns a complete new project whose root views all describe the current revision

### Requirement: Unchanged module syntax is reused exactly

When a current module has the same canonical module identity, source origin, and byte sequence as a
module in the prior project, closure loading SHALL reuse the exact immutable syntax artifact rather
than lexing and parsing it again. A changed origin or byte sequence MUST produce a new syntax
artifact.

#### Scenario: Edit one module beside an unchanged dependency

- **WHEN** a root changes but a resolved dependency retains its identity, origin, and bytes
- **THEN** the new project references the dependency's prior syntax artifact by object identity and parses a new root syntax artifact

#### Scenario: Preserve equal bytes from a changed origin

- **WHEN** a module keeps equal bytes but its source origin changes
- **THEN** the new project constructs a fresh syntax artifact rather than reusing origin-owned spans and diagnostics

### Requirement: Project syntax revision evidence is explicit

Each current project module SHALL have exactly one immutable syntax revision observation identifying
it as fresh, exactly reused, or reparsed with an adjacent-revision correspondence. Removed prior
modules SHALL not appear as current observations. Reparsed modules with no same-identity predecessor
SHALL be fresh.

#### Scenario: Observe a mixed revision

- **WHEN** a project revision contains one unchanged module, one edited module, and one newly resolved module
- **THEN** its observations identify the modules as reused, changed with correspondence, and fresh respectively

### Requirement: Syntax reuse does not reuse semantic facts prematurely

Project revision analysis SHALL recompute declaration, resolution, elaboration, ownership, tooling,
diagnostic, and root-view facts for the complete current closure even when some syntax artifacts are
reused. It MUST NOT expose semantic facts from a prior project as current merely because syntax was
reused or corresponded.

#### Scenario: Recompute one coherent frontend

- **WHEN** any module changes while other module syntax is reused
- **THEN** every current root view shares one newly completed semantic frontend and no prior semantic index is exposed as current

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

### Requirement: Only the accepted predecessor governs semantic invalidation

Semantic invalidation planning SHALL compare a revision only with the prior completed project
supplied to project revision analysis. A stale or superseded LSP computation MUST NOT become the
predecessor of a later accepted revision.

#### Scenario: Supersede an in-flight revision

- **WHEN** revision N+1 finishes after N+2 has made it stale
- **THEN** a later revision compares against the last atomically committed project rather than N+1's uncommitted surfaces or observations
