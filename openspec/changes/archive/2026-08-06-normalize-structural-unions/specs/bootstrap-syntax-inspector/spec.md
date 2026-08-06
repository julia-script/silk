## ADDED Requirements

### Requirement: Inspect structural unions in the unified workbench

The unified `/labs` workbench SHALL inspect source members, normalized types, contextual conversions,
ownership and cleanup, target layouts and calling shapes, HIR/MIR operations, evaluator values and
events, and target realization through facade queries only. Coordinated selection SHALL follow one
member or conversion across representations, and every visual relationship SHALL have an accessible
textual equivalent.

#### Scenario: Follow a widening across representations

- **WHEN** a user selects a source union widening
- **THEN** coordinated panes identify its precise source type, target members, canonical mapping, layout, MIR operation, trace, and emitted provenance

### Requirement: Structural-union presets cover valid and invalid states

Browser-local presets SHALL include order/duplicate/nesting normalization, `Never`, precise binding
inference, nominal injection, union widening, call/return boundaries, struct and array containment,
move-only transfer and cleanup, mutable replacement, invalid member types, unresolved members,
non-containing targets, and unavailable layouts without adding a standalone legacy inspector.

#### Scenario: Explore an invalid widening

- **WHEN** a preset attempts to convert a source union whose member is absent from the target
- **THEN** the workbench retains source members, expected context, failed mapping, and exact diagnostic without claiming HIR, MIR, or codegen exists

