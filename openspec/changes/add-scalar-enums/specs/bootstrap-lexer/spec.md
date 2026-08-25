## ADDED Requirements

### Requirement: Enum is a complete-identifier keyword

The lexer SHALL emit `enum` as the dedicated enum keyword only when it is a complete identifier and
SHALL retain exact source provenance under the existing trivia and recovery model.

#### Scenario: Distinguish enum from an identifier prefix

- **WHEN** source contains `enum` and `enumerate`
- **THEN** the first token is the enum keyword and the second remains one identifier token
