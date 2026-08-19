## ADDED Requirements

### Requirement: Distribution catalogs do not create module scope

The canonical distribution catalog SHALL record module source identity, digest, documentation, layer, target-provider classification, and runtime inventory metadata. Module closure and scope SHALL contain only declarations introduced through the current module and explicit imports; catalog membership SHALL NOT create an implicit prelude.

#### Scenario: Require an explicit standard-library import

- **WHEN** source references `Effect.map` without importing its module
- **THEN** analysis reports the unresolved name and completion can add the explicit module-qualified import

#### Scenario: Enforce layer direction

- **WHEN** portable standard-library source depends on a target-provider layer
- **THEN** catalog validation rejects the distribution dependency before compilation

### Requirement: Redundant imports remain valid source

Exact duplicate imports, unchanged aliases, and combinable repeated imports SHALL preserve their valid bindings and SHALL NOT cause compiler errors. The language service MAY warn and offer deterministic consolidation edits.

#### Scenario: Compile a duplicate import

- **WHEN** one module imports the same public declaration twice without a collision
- **THEN** compilation succeeds and the language service may offer removal of the redundant clause

### Requirement: Import tooling materializes every discovered dependency

Completion SHALL discover visible catalog declarations and insert a module-qualified import with collision-aware aliasing. Code actions SHALL materialize explicit Effect failure and requirement propagation, recovery, provision, and missing imports without exposing invisible bindings.

#### Scenario: Complete a colliding type name

- **WHEN** completion selects a declaration whose short name is already bound
- **THEN** it inserts a deterministic alias and uses that alias at the completion site
