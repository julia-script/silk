## ADDED Requirements

### Requirement: Type completion offers canonical integer types

Type completion SHALL derive the ordered lowercase integer vocabulary plus `bool`, `()`, and `never` from semantic identities and MUST NOT offer removed uppercase aliases.

#### Scenario: Complete an integer type

- **WHEN** completion is requested in a type position
- **THEN** all fixed- and target-width integer spellings are offered deterministically
