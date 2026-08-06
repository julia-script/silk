## ADDED Requirements

### Requirement: Mutation and loop facts are facade queries

The facade SHALL expose binding mutability, writable places, assignment compatibility and replacement,
loop identities and nesting, condition facts, lexical transfers, ownership fixed points, cleanup
outcomes, control-DAG regions and edges, writes, evaluation events, and backend provenance from one
immutable snapshot. Tooling MUST NOT reconstruct a loop from branches or infer a write target from
syntax alone.

#### Scenario: Query one loop iteration path

- **WHEN** a snapshot contains an indexed write followed by `continue`
- **THEN** facade queries link its syntax, semantic place, HIR region, ownership cleanup, MIR repeat outcome, trace, and backend branch provenance

### Requirement: Control DAG facade answers are immutable and deterministic

Facade graph answers SHALL use canonical identities and immutable ordered collections. Repeated
snapshots of identical sources and targets SHALL expose identical topological region order, shared
cleanup edges, transfer targets, and encodings without mutable graph identity.

#### Scenario: Reload a nested-loop snapshot

- **WHEN** identical nested loops are analyzed in fresh processes
- **THEN** every region, edge, write, cleanup, and encoded answer is identical
