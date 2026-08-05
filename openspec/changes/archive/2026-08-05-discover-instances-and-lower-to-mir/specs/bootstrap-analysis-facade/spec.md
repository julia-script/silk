## ADDED Requirements

### Requirement: Discovery and lowered MIR are facade queries

The facade SHALL answer the snapshot's instance discovery (entry state and ordered instances)
and its lowered MIR program as immutable values, alongside the existing queries.

#### Scenario: Query discovery and lowered MIR

- **WHEN** a snapshot's root module has a valid entry
- **THEN** the facade answers the ordered instances and a lowered MIR program containing one function per instance

#### Scenario: Answer an unavailable entry

- **WHEN** the root module has no valid entry
- **THEN** the facade answers the explicit unavailable entry state and an empty lowered program
