## ADDED Requirements

### Requirement: Canonical integer, unit, and bottom syntax is lossless

The parser SHALL preserve every lowercase integer primitive spelling, `()` in type and value positions, `never` in type positions, omitted unit results, bare `return`, and exact signed or unsigned literal tokens under existing bounded recovery rules.

#### Scenario: Parse canonical forms

- **WHEN** source contains lowercase integer declarations plus unit and bottom forms
- **THEN** syntax retains every token and exact span without deciding target width

## REMOVED Requirements

### Requirement: Never type syntax is explicit

**Reason**: Uppercase `Never` is replaced by lowercase `never` within the complete integer foundation.

**Migration**: Write `never`.

### Requirement: Usize uses existing type and literal syntax

**Reason**: Uppercase `Usize` is removed and lowercase `usize` follows the complete contextual integer syntax.

**Migration**: Write `usize` and use explicit conversions where needed.
