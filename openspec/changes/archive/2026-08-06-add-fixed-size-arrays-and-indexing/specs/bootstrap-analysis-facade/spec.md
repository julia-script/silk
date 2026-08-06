## ADDED Requirements

### Requirement: Fixed-array facts are facade queries

The facade SHALL expose canonical array types, literal elements and completeness, indexed-place
chains and bounds modes, HIR, ownership and cleanup, reachability, repeated-element layout, calling
paths, MIR, evaluation traces, and codegen outcomes from one immutable snapshot. Tooling MUST NOT
reconstruct type lengths, literal compatibility, bounds knowledge, cleanup order, or lane paths.

#### Scenario: Query one indexed value path

- **WHEN** a snapshot evaluates `pairs[index].left`
- **THEN** facade queries link syntax through emission using the same canonical array, index selector, field identity, and provenance

### Requirement: Array facade answers remain immutable and deterministic

Repeated snapshots of identical sources and targets SHALL answer identically ordered array facts and
byte-identical encodings without depending on mutable collections or JavaScript object identity.

#### Scenario: Repeat nested-array queries

- **WHEN** identical nested-array inputs are snapshotted in fresh processes
- **THEN** every array type, layout, calling path, MIR operation, and trace query answers identically
