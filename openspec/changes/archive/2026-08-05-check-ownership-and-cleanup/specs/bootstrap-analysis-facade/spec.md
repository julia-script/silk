## MODIFIED Requirements

### Requirement: One snapshot answers supported queries

The facade SHALL build one immutable analysis snapshot from a compilation request (with a
single-source convenience for one module) and SHALL answer queries over sources, syntax
artifacts, import facts and cycles, collected declarations and lookups, elaborated function facts
with their types, references, and contracts, HIR facts, and ownership facts with their cleanup
plans. Query results SHALL be immutable values, and repeated snapshots of identical input SHALL
answer every query identically.

#### Scenario: Query a multi-module snapshot

- **WHEN** a snapshot is built from a request whose root imports another module
- **THEN** the facade lists both modules, answers each module's syntax artifact and declarations, and resolves declaration lookups per module

#### Scenario: Repeat snapshot construction

- **WHEN** the same request is snapshotted repeatedly in fresh processes
- **THEN** every supported query answers identically

#### Scenario: Query ownership facts

- **WHEN** a snapshot's module contains checked functions
- **THEN** the facade answers the module's ownership facts and cleanup plans as immutable values
