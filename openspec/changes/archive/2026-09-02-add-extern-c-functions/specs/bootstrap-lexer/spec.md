## ADDED Requirements

### Requirement: Extern is a complete-identifier keyword

The lexer SHALL emit `extern` as the dedicated foreign-declaration keyword only when it is a
complete identifier and SHALL retain exact source provenance under the existing trivia and recovery
model.

#### Scenario: Distinguish extern from an identifier prefix

- **WHEN** source contains `extern` and `external`
- **THEN** the first token is the extern keyword and the second remains one identifier token
