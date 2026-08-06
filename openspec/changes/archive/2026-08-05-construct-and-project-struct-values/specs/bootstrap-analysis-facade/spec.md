## ADDED Requirements

### Requirement: Struct-value facts are facade queries

The facade SHALL expose struct literal target and field mappings, projection chains, typed aggregate
HIR, whole-value ownership and cleanup, aggregate runtime reachability, catalog and calling-shape
facts, aggregate MIR, evaluation traces, and codegen outcomes from one immutable snapshot. Every
query SHALL reuse the same canonical nominal and field identities; tooling MUST NOT reconstruct
construction completeness, projection lookup, ownership, lane order, or backend realization.

#### Scenario: Query one complete struct value path

- **WHEN** a snapshot constructs a struct through a factory and projects a nested scalar
- **THEN** facade queries link syntax, semantic mappings, HIR, ownership, layout, MIR, evaluation, and emission through the same canonical nominal and field identities

#### Scenario: Query an unavailable construction

- **WHEN** a literal is externally unauthorized, incomplete, duplicated, or mistyped
- **THEN** the facade retains every supplied field fact and exact cause while aggregate HIR, MIR, evaluation, and codegen remain explicitly unavailable

### Requirement: Aggregate facade answers remain immutable and deterministic

Facade results containing aggregate values or paths SHALL use immutable canonical data rather than
JavaScript object identity or mutable maps. Repeated snapshots of identical inputs SHALL answer
byte-identical encodings and identically ordered aggregate facts.

#### Scenario: Repeat aggregate queries

- **WHEN** identical nested aggregate sources and targets are snapshotted in fresh processes
- **THEN** every construction, projection, ownership, layout, MIR, and trace query answers identically
