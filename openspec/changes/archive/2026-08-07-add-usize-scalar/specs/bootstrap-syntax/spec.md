## ADDED Requirements

### Requirement: Usize uses existing type and literal syntax

The syntax layer SHALL preserve `Usize` as an ordinary type path and decimal literals as their exact
source tokens without introducing a numeric suffix or target-dependent syntax node. Damaged type
positions and surrounding expressions SHALL retain the existing bounded recovery behavior.

#### Scenario: Parse a Usize parameter

- **WHEN** a function declares a parameter or result type spelled `Usize`
- **THEN** parsing preserves the type path and every source span using the existing function grammar
