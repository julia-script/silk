## ADDED Requirements

### Requirement: Inspect fixed arrays in the unified workbench

The unified `/labs` workbench SHALL inspect array type syntax, literal source and canonical element
order, inferred or contextual types, indexed place chains, bounds modes, ownership and cleanup,
repeated-element layout and ABI paths, MIR, evaluation events, and target-selected artifacts through
facade queries only. Every visual relationship SHALL have an accessible textual equivalent.

#### Scenario: Inspect a dynamic array index

- **WHEN** a preset indexes an array with an `I32` parameter
- **THEN** coordinated panes show the required bounds check and link its syntax, HIR, MIR, trace, and emitted branch provenance

### Requirement: Fixed-array presets cover the complete slice

Browser-local presets SHALL include inferred, contextual, empty, nested, struct-element, reordered
evaluation, whole-moved, Copy-read, indexed-field, constant-out-of-bounds, dynamic-trap, type-mismatch,
length-mismatch, partial-move, and unavailable-layout cases without adding a standalone legacy lab.

#### Scenario: Explore invalid array states

- **WHEN** a user selects an invalid array preset
- **THEN** the unified workbench retains every available fact and exact phase-owned cause without claiming a later artifact exists
