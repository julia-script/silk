## ADDED Requirements

### Requirement: Module surfaces encode nominal union contracts

A module semantic surface SHALL encode every externally observable nominal-union header fact,
including canonical parent identity, declaration kind, visibility, ordered parameters, ordered
variant identities and kinds, ordered payload fields, field visibility and types, explicit validity,
and generic bounds. It SHALL exclude numeric tags, target layout, bodies, and source spans. Exact
surface equality and dependency invalidation SHALL treat any change to that semantic shape exactly as
an observable nominal struct-shape change.

#### Scenario: Round-trip a public generic union surface

- **WHEN** a module exports `Result<A, E>` with unit or named-field variants
- **THEN** encode and decode preserve the complete ordered parent, parameter, variant, field, visibility, bound, and availability contract

#### Scenario: Invalidate a dependent after a payload change

- **WHEN** an exported variant adds, removes, reorders, renames, or changes the type or visibility of a field
- **THEN** the module surface changes and every direct dependent is selected for dependency-surface recomputation

#### Scenario: Ignore implementation-only edits

- **WHEN** a factory function body changes without changing the exported union contract
- **THEN** the union portion of the module surface remains equal and does not independently invalidate dependents
