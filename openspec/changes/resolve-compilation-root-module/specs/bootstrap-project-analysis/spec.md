## MODIFIED Requirements

### Requirement: One project revision analyzes the union module closure once

Project frontend analysis SHALL accept one or more canonical root module strings, resolve their source bytes
and the union of their reachable module closures through the supplied resolver, and process each canonical module at most once for that project
revision. Root supply order SHALL NOT change module order, diagnostics, facts, or observations.

#### Scenario: Roots share a dependency

- **WHEN** two project roots both reach the same imported module
- **THEN** the project analysis loads, parses, indexes, elaborates, and ownership-checks that module once while retaining both roots

#### Scenario: Repeat roots in another order

- **WHEN** equivalent root identities are supplied in a different order
- **THEN** the project analysis exposes identically ordered modules, facts, diagnostics, and deterministic observation counts

#### Scenario: Resolve current editor overlays

- **WHEN** the current revision resolver supplies unsaved bytes for a project root and a dependency
- **THEN** analysis uses those bytes for both modules through the same resolver while preserving their source origins

#### Scenario: Deduplicate root identities

- **WHEN** the request repeats a canonical root identity
- **THEN** the project exposes one root view and resolves that module at most once per closure load
