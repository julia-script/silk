## ADDED Requirements

### Requirement: Unified labs explain Effect and owned allocation

The unified `/labs` workbench SHALL provide coordinated presets for eager versus lazy Effect code,
capture repeatability, catch and retry, provider placement, Layout, raw storage, Vector growth,
failure rollback, ownership, Drop, MIR, evaluator traces, and native/Wasm realization. It MUST NOT add
a disconnected allocation inspector or present named Scope as current language behavior.

#### Scenario: Follow a failed Vector append

- **WHEN** a developer opens the injected-exhaustion preset
- **THEN** linked panes show source, Effect contract, ownership transfer, layout, MIR failure branch, rollback trace, and both backend realizations with accessible text equivalents
