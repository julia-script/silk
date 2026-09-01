## ADDED Requirements

### Requirement: Text and byte literals parse losslessly

The parser SHALL preserve quote delimiters, byte prefix, content, escapes, trivia, recovery elements, and exact spans without decoding storage during syntax construction.

#### Scenario: Recover a malformed escape

- **WHEN** a literal contains a malformed escape
- **THEN** damage remains local and the following statement remains parseable
