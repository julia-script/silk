## ADDED Requirements

### Requirement: Pointer types have one canonical layout

The formatter SHALL print pointer types as `*const <type>` and `*mut <type>` with no space after
the star and one space before the pointee, idempotently.

#### Scenario: Format a pointer type

- **WHEN** source spells `* mut   u8`
- **THEN** formatting yields `*mut u8` and a second pass is byte-identical
