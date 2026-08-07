## ADDED Requirements

### Requirement: Unified Labs inspection exposes flow outcomes

The `/labs` workbench SHALL include one canonical flow preset and coordinate syntax, declaration,
semantic, ownership, HIR, instances, layout, MIR, evaluator, native, and Wasm projections. A stopped
path MUST remain visibly unavailable rather than fabricating downstream success.

#### Scenario: Inspect handled and unhandled rows

- **WHEN** the preset is changed between exact recovery and a residual failure
- **THEN** the inspector shows the selected row subtraction and either the executable path or the phase-owned stop
