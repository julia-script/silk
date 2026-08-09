## ADDED Requirements

### Requirement: Project views are queryable but not directly realizable

The analysis facade SHALL distinguish a single-root frontend snapshot from a project-analysis root
view in its public types. Both SHALL support frontend queries. Runtime realization SHALL accept only
the single-root frontend snapshot; a project view MUST require an explicit future project
realization contract rather than being accepted accidentally through structural typing.

#### Scenario: Query a project root view

- **WHEN** the LSP receives a root view from one completed multi-root project analysis
- **THEN** it can issue all supported frontend queries against that view without runtime realization

#### Scenario: Reject direct project-view realization

- **WHEN** a typed caller attempts to pass a project root view to runtime realization
- **THEN** the compiler API rejects the call at type-check time

#### Scenario: Realize a single-root frontend

- **WHEN** a caller builds a single-root frontend snapshot and requests runtime realization
- **THEN** the facade accepts it and derives runtime facts from exactly that snapshot
