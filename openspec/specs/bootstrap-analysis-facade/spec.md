# bootstrap-analysis-facade Specification

## Purpose

The immutable, snapshot-backed query surface over compiler analysis used by editor tooling.

## Requirements

### Requirement: One snapshot answers static queries

The facade SHALL answer source, syntax, import, declaration, reference, type, contract, HIR, MIR,
ownership, target-profile, selected-root, native-requirement, and diagnostic queries from immutable
snapshots. Repeated analysis of identical input SHALL produce identical answers.

#### Scenario: Query an incomplete project

- **WHEN** a project revision contains recoverable syntax or semantic damage
- **THEN** unaffected facts remain queryable and dependent facts expose explicit unavailable states

### Requirement: Analysis never executes user code

Normal analysis, hover, diagnostics, indexing, and inspection SHALL perform no runtime execution and
SHALL expose no runtime value, outcome, trace, blocked reason, terminal, host adapter, or execution
request. Compile-time `StaticEvaluation` MAY run only where language analysis requires it.

#### Scenario: Inspect a valid program

- **WHEN** a tool requests every supported inspection view
- **THEN** the facade returns static compiler facts without starting an executable artifact

### Requirement: Tooling uses the facade

Tooling SHALL consume compiler facts through this facade rather than reconstructing Silk semantics
from syntax or invoking internal compiler phases directly.

#### Scenario: Render project facts

- **WHEN** the language server renders a project revision
- **THEN** it derives the response entirely from facade queries and merged diagnostics
