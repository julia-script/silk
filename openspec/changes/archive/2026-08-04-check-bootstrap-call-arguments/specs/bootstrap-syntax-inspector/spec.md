## ADDED Requirements

### Requirement: Inspect call argument contracts
The Syntax Inspector SHALL show every ordered argument fact, its expression and type, its positional
target parameter when available, and the complete call-contract state. Presets SHALL cover a
compatible call, too few arguments, too many arguments, an unavailable mapped type, and an
unresolved call while retaining the existing syntax, relationship, return compatibility, and
phase-separated diagnostic views.

#### Scenario: Inspect a compatible call contract
- **WHEN** a developer selects the `identity(42)` preset
- **THEN** the inspector shows argument zero mapped to `identity` parameter zero with available `I32` types and a compatible contract

#### Scenario: Inspect wrong arity
- **WHEN** a developer selects a too-few or too-many preset
- **THEN** expected and actual counts, any positionally available mappings, and `SEM0007` are visible without hiding the resolved call target

#### Scenario: Inspect an unavailable contract
- **WHEN** a developer selects a preset with an unresolved argument type or call target
- **THEN** the inspector explains which prerequisite is unavailable and does not display an invented binding or mismatch
