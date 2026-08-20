## MODIFIED Requirements

### Requirement: Structural union facts are facade queries

The analysis facade SHALL expose source union members, canonical normalized ordinary types,
`never`, expected contexts, injection/widening outcomes and exact mappings, ownership
classification, active-member cleanup, instance reachability, target layouts, calling shapes,
HIR/MIR conversions, evaluation values and events, and backend provenance from one immutable
snapshot. Tooling MUST NOT normalize members, assign tags, infer conversions, reconstruct
executable representations, or decode payload storage independently.

#### Scenario: Query one injection across the pipeline

- **WHEN** a scalar, array, nominal, exact callable, or opaque Effect value is contextually returned as a union
- **THEN** facade queries link its exact source member through semantic conversion, HIR, ownership, layout, MIR, evaluation, and both backend artifacts
