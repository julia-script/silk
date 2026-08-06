## ADDED Requirements

### Requirement: Struct declarations and layouts are facade queries

The facade SHALL expose every module's nominal struct headers, ordered field facts, type lookups,
visibility and dependency states, the snapshot's complete nominal layout catalog, and its
reachable runtime layout plan as immutable queries. Struct facts and layout entries SHALL reuse the
same canonical nominal identity; tooling MUST NOT reconstruct fields, dependencies, recursion,
padding, or offsets from syntax.

#### Scenario: Query a complete nominal struct

- **WHEN** a snapshot contains an available struct with scalar fields
- **THEN** the facade returns its canonical header, ordered resolved fields, and selected-target catalog entry under one nominal identity

#### Scenario: Query a cross-module field dependency

- **WHEN** one struct contains a public nominal type imported from another module
- **THEN** the field lookup and both layout entries expose the same canonical imported type identity

#### Scenario: Query an unavailable recursive layout

- **WHEN** structs form an inline recursive dependency cycle
- **THEN** the facade retains their headers and fields while exposing unavailable layout states and the canonical diagnostic cause
