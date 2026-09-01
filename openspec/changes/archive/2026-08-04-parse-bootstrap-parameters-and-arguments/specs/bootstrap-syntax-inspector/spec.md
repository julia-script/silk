## ADDED Requirements

### Requirement: Inspect parameter and argument syntax

The Syntax Inspector SHALL provide valid and malformed presets for typed parameter declarations,
bare-identifier expressions, and value-carrying calls. It SHALL show the concrete parameter and
argument branches, every separator and token span, local recovery nodes, exact declaration parameter
counts, and the explicitly deferred semantic state.

#### Scenario: Inspect the identity syntax slice

- **WHEN** a developer selects the parameter-and-argument preset
- **THEN** the concrete view shows `value: I32`, the returned `value`, and the `42` in `identity(42)` while semantic panels clearly mark parameter resolution and argument checking as deferred

#### Scenario: Inspect malformed list recovery

- **WHEN** a developer selects a preset with a missing parameter type, comma, or call parenthesis
- **THEN** the relevant missing token and parser diagnostic remain visible while following syntax still renders
