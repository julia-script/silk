## ADDED Requirements

### Requirement: Target planning owns allocation and Effect physical facts

Target layout SHALL compute validated byte and repeated-element layouts, raw-buffer lanes, reclaim
ticket shapes, concrete Vector layouts, Effect outcome shapes, and Drop calling shapes before MIR.
Evaluator and backends SHALL consume those facts unchanged and MUST NOT derive stride, alignment,
failure transport, or cleanup representation independently.

Target layout SHALL separately plan each reachable hidden Effect capture environment. Borrowed
captures use target-width provenance-bearing references; Copy and moved captures use their ordinary
value layouts. Effect values and Effect outcomes are distinct physical facts.

#### Scenario: Plan the same Vector for native and Wasm

- **WHEN** `Vector<Token>` is reachable for a 64-bit native target and `wasm32`
- **THEN** each target receives one compiler-owned address-width layout while retaining identical logical ownership and cleanup semantics
