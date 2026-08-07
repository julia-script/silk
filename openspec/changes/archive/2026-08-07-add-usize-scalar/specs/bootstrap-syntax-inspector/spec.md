## ADDED Requirements

### Requirement: The unified inspector exposes Usize across targets

The `/labs` workbench SHALL include one canonical `Usize` preset and coordinate syntax, semantic
facts, operators, HIR, instances, selected target layout, MIR, evaluator, native, and Wasm
projections. Switching between native and Wasm targets SHALL visibly change width and availability
without changing the canonical source type or fabricating downstream results.

#### Scenario: Compare target width in Labs

- **WHEN** a user switches the canonical `Usize` preset between a native target and Wasm
- **THEN** coordinated projections show the selected word width, exact literal verdict, calling lane, and stopped Wasm path for native-only magnitudes
