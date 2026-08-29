## ADDED Requirements

### Requirement: Union is a complete-identifier keyword

The lexer SHALL emit `union` as the dedicated nominal-union keyword only when it is a complete
identifier and SHALL retain exact source provenance under the existing trivia and recovery model.

#### Scenario: Distinguish union from an identifier prefix

- **WHEN** source contains `union` and `unionize`
- **THEN** the first token is the union keyword and the second remains one identifier token
