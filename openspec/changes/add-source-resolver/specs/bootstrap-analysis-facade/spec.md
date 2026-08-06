## ADDED Requirements

### Requirement: Resolver-backed snapshots remain useful around operational failure

The facade SHALL build snapshots using the source-resolution capability and SHALL expose the exact
loaded source catalog, failed import facts, and canonically ordered source-resolution failures.
Snapshot construction SHALL capture imported-source resolution failures as immutable analysis data
rather than failing the whole tooling operation. Successfully loaded modules and every unrelated
syntax, declaration, name-resolution, HIR, ownership, target, and layout fact SHALL remain
queryable through the same snapshot.

#### Scenario: Query around an unreadable imported module

- **WHEN** one imported module fails resolution while another imported module loads successfully
- **THEN** the snapshot exposes the typed failure and failed import while the root and successful module remain queryable

#### Scenario: Render diagnostics from every loaded source

- **WHEN** several loaded modules produce diagnostics
- **THEN** the facade's source catalog contains the exact bytes for every diagnostic source identity needed to compute its location

#### Scenario: Build a browser snapshot from virtual sources

- **WHEN** browser tooling provides an in-memory resolver for a multi-module project
- **THEN** the facade builds and answers the same snapshot queries without requiring filesystem services

### Requirement: Emission refuses an invalid snapshot

Backend emission through the facade SHALL be unavailable when the snapshot contains any error
diagnostic or source-resolution failure. Refusing emission SHALL retain the snapshot's diagnostics
and resolution failures and SHALL NOT invoke a backend.

#### Scenario: Refuse emission after source rejection

- **WHEN** a snapshot contains a missing-module or semantic error diagnostic
- **THEN** its codegen query is unavailable and does not invoke the selected backend
#### Scenario: Refuse emission after resolver failure

- **WHEN** a snapshot records an operational source-resolution failure
- **THEN** its codegen query is unavailable and does not invoke the selected backend
