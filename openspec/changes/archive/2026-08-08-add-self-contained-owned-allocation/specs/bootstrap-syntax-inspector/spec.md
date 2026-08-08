## ADDED Requirements

### Requirement: Unified Labs exposes allocation ownership and rollback

The existing `/labs` workbench SHALL add coordinated presets for successful allocation, deterministic
exhaustion, provider access ending before owner cleanup, partial initialization rollback, explicit
early drop, invalid unsafe access, invalid Drop hooks, zero-sized ownership, and target-specific
layout. Source, syntax, semantic facts, ownership, HIR, instances, layout, MIR, evaluator traces,
native output, and Wasm output SHALL remain synchronized through the analysis facade. The workbench
MUST NOT add a disconnected allocation inspector or present named Scope, Arena privilege, or Vector
as implemented by this change.

#### Scenario: Follow an exhausted allocation

- **WHEN** a developer opens the injected-exhaustion preset
- **THEN** linked panes show the allocator requirement, validated layout, typed failure branch, cleanup of earlier owners, absence of a rejected owner, and matching native and Wasm behavior

#### Scenario: Follow partial rollback

- **WHEN** a developer opens the guarded-prefix failure preset
- **THEN** linked panes show each initialized slot, the guard's Drop hook, element destruction before byte release, and the unchanged propagated failure

#### Scenario: Explain the deliberate boundary

- **WHEN** a developer inspects the feature summary
- **THEN** accessible text states that allocator implementations are ordinary capabilities and that arena-backed escaping results, named lifetime scopes, and Vector remain outside this change
