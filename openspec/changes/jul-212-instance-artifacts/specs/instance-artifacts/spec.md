# instance-artifacts Specification

## ADDED Requirements

### Requirement: Instantiation has explicit inputs

The compiler SHALL expose `Realization.instantiate` over checked module results, declaration index,
resolution, completed profile, selected roots, and runtime composition without accepting a frontend.

#### Scenario: Standalone instantiation

- **WHEN** equivalent explicit inputs are supplied outside the frontend coordinator
- **THEN** the operation returns the same deterministic reachable instance graph

### Requirement: Instance graphs are portable artifacts

The returned instance graph SHALL NOT retain a source resolver, semantic session, frontend,
presentation registry, or process-global lookup capability.

#### Scenario: Presentation-independent comparison

- **WHEN** two snapshots have equivalent semantics and different presentations
- **THEN** their instance artifacts compare equal without stripping live fields

### Requirement: Presentation consumers are explicit

Diagnostics and lowering that require current spans SHALL receive presentation separately from the
instance artifact.

#### Scenario: Instance diagnostic

- **WHEN** a reachable instance violates a target rule
- **THEN** the diagnostic consumer resolves its anchor through its explicit registry input
