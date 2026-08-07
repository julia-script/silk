## ADDED Requirements

### Requirement: Effect and owned-allocation syntax is lossless and recoverable

The syntax layer SHALL preserve `effect {}`, `effect fn`, `Effect` actor calls, `run`, Copy or moved
`fail`, allocator requirements, explicit consuming `drop`, restricted Drop declarations, and
qualified unsafe allocation/buffer operations. The lexer SHALL reserve `effect` and cease treating
`flow` as the effect-function keyword.

#### Scenario: Recover a damaged effect allocation body

- **WHEN** source contains an effect function with a damaged unsafe allocation call followed by a valid `Effect.catch` pipeline
- **THEN** the syntax tree retains bounded damaged allocation nodes and the later Effect pipeline without reverting to Flow or Scope nodes
