## ADDED Requirements

### Requirement: Exhaustive-match facts are facade queries

The analysis facade SHALL expose match syntax identities, scrutinee and access facts, source arms,
canonical member coverage before and after each arm, pattern paths and bindings, guard outcomes,
narrowed types, result joins, ownership and cleanup, reachability, HIR/MIR regions, evaluation events,
and backend provenance from one immutable snapshot. Tooling MUST NOT reconstruct coverage, infer
narrowing, select payload fields, or decode physical tags independently.

#### Scenario: Query one guarded match across the pipeline

- **WHEN** a nominal member passes a false guard and is handled by a later consuming arm
- **THEN** facade queries link the source arms, canonical coverage, payload binding, cleanup, MIR decision, trace, and both backend artifacts

### Requirement: Match facade answers are immutable and deterministic

Facade match answers SHALL use stable syntax, region, member, field, and binding identities with
immutable ordered collections. Repeated snapshots of equivalent source SHALL expose identical
coverage sets, mappings, joins, cleanup, traces, and encodings without mutable graph identity.

#### Scenario: Reload an exhaustive match

- **WHEN** equivalent matches are analyzed repeatedly in fresh snapshots
- **THEN** every source-ordered decision and canonical cross-phase answer agrees exactly
