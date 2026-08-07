## ADDED Requirements

### Requirement: Backends realize self-contained allocation and Effect parity

Native LLVM and direct Wasm SHALL realize compiler-planned Effect outcomes, allocator witness calls,
self-contained reclaim tickets, raw-buffer operations, Vector moves, and Drop order from verified MIR.
Neither backend may choose layout, turn `OutOfMemory` into a trap, recognize an allocator kind, or
introduce a lifetime scope absent from MIR.

#### Scenario: Agree on successful and exhausted growth

- **WHEN** equivalent native and Wasm programs grow a Vector successfully and under injected exhaustion
- **THEN** both match evaluator results, failure members, element state, and cleanup traces for their selected target layouts
