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

