## ADDED Requirements

### Requirement: Type is a complete-identifier keyword

The lexer SHALL emit `type` as the dedicated type-alias keyword only when it is a complete
identifier and SHALL retain exact source provenance under the existing trivia and recovery model.

#### Scenario: Distinguish type from an identifier prefix

- **WHEN** source contains `type` and `typeName`
- **THEN** the first token is the type keyword and the second remains one identifier token
