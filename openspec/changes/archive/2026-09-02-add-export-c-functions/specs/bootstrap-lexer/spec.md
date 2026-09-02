## ADDED Requirements

### Requirement: Export is a complete-identifier keyword

The lexer SHALL emit `export` as the dedicated native-export keyword only when it is a complete
identifier and SHALL retain exact source provenance under the existing trivia and recovery model.

#### Scenario: Distinguish export from an identifier prefix

- **WHEN** source contains `export` and `exported`
- **THEN** the first token is the export keyword and the second remains one identifier token
