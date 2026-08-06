## ADDED Requirements

### Requirement: Fixed-array source types have one canonical bracketed layout

The formatter SHALL print every complete fixed-array source type with no space immediately inside
either bracket, no space before the semicolon, and one space after the semicolon. Nested fixed-array
types SHALL apply the same layout recursively. Missing required fixed-array punctuation SHALL remain
damaged syntax and SHALL NOT be repaired by formatting.

#### Scenario: Format a fixed-array type

- **WHEN** complete source spells a fixed-array type with noncanonical trivia as `[ I32 ;4 ]`
- **THEN** formatting emits `[I32; 4]`

#### Scenario: Format a nested fixed-array type

- **WHEN** complete source contains a nested fixed-array type equivalent to `[[I32; 4]; 3]`
- **THEN** formatting emits `[[I32; 4]; 3]` with the canonical layout at both levels

#### Scenario: Refuse to repair a missing separator

- **WHEN** a fixed-array type is missing its required semicolon
- **THEN** formatting rejects the damaged syntax instead of inserting the semicolon
