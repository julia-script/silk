## ADDED Requirements

### Requirement: The unified inspector exposes scoped allocation coherently

The `/labs` workbench SHALL include canonical successful and exhaustion scoped-allocation presets and
coordinate syntax, semantic facts, ownership, HIR, instances, target layout, MIR, drop-hook and cleanup events,
native realization, and Wasm realization around the same selected allocation and scope identities.
Unavailable or rejected paths SHALL stop downstream projections explicitly rather than displaying
fabricated storage or cleanup.

#### Scenario: Follow one allocation across projections

- **WHEN** a user selects an allocation in the canonical successful preset
- **THEN** coordinated projections expose its source span, scope, allocator role, layout, ownership transitions, MIR operations, backend storage, and exactly-once release event

#### Scenario: Inspect an exhausted allocation

- **WHEN** a user opens the canonical exhaustion preset
- **THEN** the workbench shows `OutOfMemory`, cleanup of prior allocations, absence of a resource for the rejected request, and stopped invalid downstream paths
