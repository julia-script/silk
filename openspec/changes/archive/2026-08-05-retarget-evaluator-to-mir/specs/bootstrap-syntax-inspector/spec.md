## ADDED Requirements

### Requirement: The Evaluated layer replays MIR traces

The inspector's Evaluated layer and evaluation panel SHALL replay the MIR interpreter's trace
through the analysis facade: entry, call, binding, and return events matched to the static flow
view by provenance spans and canonical function identities, with exact values and evaluation
order overlaid, and blocked outcomes — including lowered traps and recursive cycles — shown with
their reasons and provenance.

#### Scenario: Overlay a completed MIR trace

- **WHEN** a developer evaluates a program whose `main` calls `identity(42)`
- **THEN** the Evaluated layer overlays the call order and the exact bound and returned values from the MIR trace

#### Scenario: Show a trap-blocked outcome

- **WHEN** the evaluated program reaches a function lowered to a generated trap
- **THEN** the panel shows the blocked outcome with the trap's reason and causative span and only the completed trace prefix
