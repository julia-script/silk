## ADDED Requirements

### Requirement: Inspect exhaustive matches in the unified workbench

The unified `/labs` workbench SHALL inspect match modes, nominal and universal patterns, nested field
bindings, guards, canonical remaining-member sets, narrowed types, joined results, ownership and
cleanup, HIR/MIR regions, evaluator decisions, and target realization through facade queries only.
Coordinated selection SHALL follow an arm or binding across existing panes, and every relationship
SHALL have an accessible textual equivalent.

#### Scenario: Follow one selected arm across representations

- **WHEN** a user selects a consuming `Token` arm
- **THEN** coordinated panes identify its source pattern, coverage transition, narrowed payload, bound fields, cleanup, MIR region, trace events, and emitted provenance

### Requirement: Match presets cover valid and invalid states

Browser-local presets SHALL include precise nominal matching, Copy and consuming union matches,
shared and exclusive modes, nested and renamed bindings, `..`, guarded fallthrough, `_`, nominal and
union result joins, match inside loops and aggregate flows, incomplete and unreachable coverage,
unknown members and fields, non-`Bool` guards, incompatible results, borrow escape, and cleanup
failures without adding a standalone inspector.

#### Scenario: Explore incomplete coverage

- **WHEN** a preset omits one unguarded union member
- **THEN** the workbench retains every arm and remaining-member transition beside the exact diagnostic without claiming executable HIR, MIR, evaluation, or codegen exists
