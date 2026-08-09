# bootstrap-module-tooling Specification

## Purpose

Defines immutable module-local editor indexes that can be structurally shared across adjacent
accepted project revisions and safely composed into current-project query indexes.

## Requirements

### Requirement: Each module owns one reusable tooling artifact

Project frontend analysis SHALL publish one immutable tooling artifact per current module containing
that module's semantic occurrences and anonymous-expression type entries. The artifact SHALL depend
on exactly one module semantic artifact and SHALL remain independent of project root selection.

#### Scenario: Inspect one module artifact

- **WHEN** a module completes semantic analysis and tooling indexing
- **THEN** its tooling artifact contains only entries whose source spans belong to that module

#### Scenario: Share an unchanged module

- **WHEN** an adjacent accepted project structurally shares a module semantic artifact and retains the corresponding prior tooling artifact
- **THEN** the current project retains that exact tooling artifact by object identity

### Requirement: Tooling reuse follows semantic artifact reuse

A prior module tooling artifact SHALL be reused only when it belongs to the same canonical module
and the current project shares its exact semantic input artifact. A recomputed semantic artifact or
missing prior tooling artifact SHALL cause both module tooling indexes to be recomputed.

#### Scenario: Reuse after an unrelated edit

- **WHEN** module `C` changes while modules `A` and `B` retain their semantic artifacts
- **THEN** `A` and `B` retain their prior tooling artifacts and `C` receives a new tooling artifact

#### Scenario: Recompute after a dependency contract edit

- **WHEN** a dependency contract edit causes an importer semantic artifact to be recomputed
- **THEN** the importer receives newly computed semantic-occurrence and anonymous-expression indexes

#### Scenario: Recover missing prior tooling

- **WHEN** a reusable semantic artifact has no matching prior tooling artifact
- **THEN** the current project recomputes that module's tooling rather than exposing an incomplete index

### Requirement: Project composition preserves current navigation

The project semantic-occurrence index SHALL compose current module indexes and resolve declaration
locations through the current set of module artifacts. A reused occurrence index MUST NOT force a
reference query to expose a predecessor declaration span.

#### Scenario: Move a dependency declaration

- **WHEN** dependency `A` receives new semantics and tooling after a source-only declaration move while importer `B` reuses its tooling artifact
- **THEN** a definition query from `B` resolves through `A`'s current tooling artifact to the current declaration span

#### Scenario: Compose multiple roots

- **WHEN** multiple roots share modules in one project revision
- **THEN** every root view shares one composed occurrence index with one entry per canonical current module

### Requirement: Tooling reuse observations are deterministic

Semantic-occurrence and anonymous-expression phases SHALL each report exact reused and recomputed
module totals. Equivalent inputs and predecessor facts SHALL produce identical totals independent of
root order and elapsed time.

#### Scenario: Observe a mixed tooling revision

- **WHEN** a revision reuses two module tooling artifacts and recomputes one
- **THEN** both tooling phase observations report exactly two reused and one recomputed module
