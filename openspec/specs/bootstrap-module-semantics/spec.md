# bootstrap-module-semantics Specification

## Purpose

Defines independently owned immutable module semantic artifacts that can be shared safely across
adjacent accepted project revisions when every local and dependency semantic input is unchanged.

## Requirements

### Requirement: Each analyzed module owns one closed semantic artifact

Project frontend analysis SHALL publish one immutable semantic artifact per current module. The
artifact SHALL own the module's elaboration, HIR, ownership facts, and phase diagnostics without
retaining the project-wide declaration index that happened to construct it. Its observable facts
MUST remain valid when embedded in a later project whose proven semantic inputs are equal.

#### Scenario: Inspect artifact ownership

- **WHEN** a project module completes elaboration and ownership analysis
- **THEN** its semantic artifact contains those module facts and no reference to the closure-wide declaration index

#### Scenario: Preserve the prior artifact

- **WHEN** an adjacent project structurally shares one module semantic artifact
- **THEN** the prior project and current project both remain immutable and expose the exact shared artifact by object identity

### Requirement: Reuse requires a reusable semantic classification

A prior module semantic artifact SHALL be reused only when the current semantic invalidation plan
classifies that module as reusable and the prior artifact belongs to the same canonical module.
Fresh, locally changed, dependency-invalidated, environment-invalidated, and cyclic-peer-invalidated
modules SHALL receive newly computed artifacts. Missing prior artifacts SHALL force recomputation.

#### Scenario: Reuse an unrelated module

- **WHEN** an accepted revision edits unrelated module `C` while modules `A` and `B` remain semantically reusable
- **THEN** `A` and `B` retain their exact prior semantic artifacts and `C` receives a new artifact

#### Scenario: Reuse an importer after a dependency body edit

- **WHEN** only the body of dependency `A` changes and importer `B` has unchanged local syntax and an equal dependency surface
- **THEN** `A` receives a new semantic artifact while `B` retains its exact prior semantic artifact

#### Scenario: Recompute an importer after a dependency contract edit

- **WHEN** a dependency signature, visibility, struct contract, conformance, or unavailable state changes
- **THEN** every importer classified as dependency-invalidated receives a new semantic artifact

#### Scenario: Recompute a cyclic component

- **WHEN** one member of an import-cycle component is locally or dependency invalidated
- **THEN** every current member of that invalidated component receives a new semantic artifact

### Requirement: Shared facts resolve presentation through the current project

Source navigation and other source-backed presentation derived from a shared semantic artifact
SHALL resolve canonical declaration and field identities against the current project's declaration
facts. It MUST NOT expose a predecessor span merely because an equivalent dependency contract was
captured by the shared artifact.

#### Scenario: Move a dependency declaration without changing its contract

- **WHEN** dependency `A` changes only source placement while importer `B` remains semantically reusable
- **THEN** a query in `B` navigates to `A`'s current declaration span rather than its predecessor span

#### Scenario: Keep local source facts exact

- **WHEN** a module artifact is shared because its syntax and semantic inputs are unchanged
- **THEN** its module-local syntax, diagnostics, HIR provenance, and ownership spans remain exact for the current project

### Requirement: Semantic reuse observations are deterministic

The semantic phases SHALL report exact deterministic totals for reused and recomputed module
artifacts. Correctness tests SHALL observe classifications, phase executions, counters, and object
sharing without relying on elapsed-time thresholds.

#### Scenario: Observe a mixed revision

- **WHEN** a project revision reuses three module artifacts and recomputes two
- **THEN** elaboration and ownership phase observations report exactly three reused and two recomputed modules in deterministic form

#### Scenario: Repeat a revision with another root order

- **WHEN** equivalent roots and prior facts are supplied in another iteration order
- **THEN** semantic reuse decisions, counters, diagnostics, and resulting facts are identical

### Requirement: Distribution catalogs do not create module scope

The canonical distribution catalog SHALL record module source identity, digest, documentation,
layer, target-provider classification, and runtime inventory metadata. Module closure and scope
SHALL contain only declarations introduced through the current module and explicit imports;
catalog membership SHALL NOT create an implicit prelude.

#### Scenario: Require an explicit standard-library import

- **WHEN** source references `Effect.map` without importing its module
- **THEN** analysis reports the unresolved name and completion can add the explicit module-qualified import

#### Scenario: Enforce layer direction

- **WHEN** portable standard-library source depends on a target-provider layer
- **THEN** catalog validation rejects the distribution dependency before compilation

### Requirement: Redundant imports remain valid source

Exact duplicate imports, unchanged aliases, and combinable repeated imports SHALL preserve their
valid bindings and SHALL NOT cause compiler errors. The language service MAY warn and offer
deterministic consolidation edits.

#### Scenario: Compile a duplicate import

- **WHEN** one module imports the same public declaration twice without a collision
- **THEN** compilation succeeds and the language service may offer removal of the redundant clause

### Requirement: Import tooling materializes every discovered dependency

Completion SHALL discover visible catalog declarations and insert a module-qualified import with
collision-aware aliasing. Code actions SHALL materialize explicit Effect failure and requirement
propagation, recovery, provision, and missing imports without exposing invisible bindings.

#### Scenario: Complete a colliding type name

- **WHEN** completion selects a declaration whose short name is already bound
- **THEN** it inserts a deterministic alias and uses that alias at the completion site
