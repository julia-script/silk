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
