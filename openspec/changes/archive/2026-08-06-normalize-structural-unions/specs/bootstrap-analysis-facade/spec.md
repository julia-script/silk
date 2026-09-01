## ADDED Requirements

### Requirement: Structural union facts are facade queries

The analysis facade SHALL expose source union members, canonical normalized types, `Never`, expected
contexts, injection/widening outcomes and mappings, ownership classification, active-member cleanup,
instance reachability, target layouts, calling shapes, HIR/MIR conversions, evaluation values and
events, and backend provenance from one immutable snapshot. Tooling MUST NOT normalize members,
assign tags, infer conversions, or decode payload storage independently.

#### Scenario: Query one injection across the pipeline

- **WHEN** a nominal value is contextually returned as a union
- **THEN** facade queries link its source member through semantic conversion, HIR, ownership, layout, MIR, evaluation, and both backend artifacts

### Requirement: Union facade answers are immutable and deterministic

Facade union answers SHALL use canonical identities and immutable ordered collections. Equivalent
source spellings and repeated fresh snapshots SHALL expose identical member order, mappings, layouts,
cleanup cases, traces, and encodings without mutable object identity.

#### Scenario: Reload a permuted union

- **WHEN** equivalent programs differ only in union member order and duplicate nesting
- **THEN** their canonical facade answers agree while each snapshot retains its own exact source syntax
