## ADDED Requirements

### Requirement: Module documentation comments are distinct trivia

The lexer SHALL classify a line comment beginning with exactly `//!` as a module-documentation
comment token distinct from ordinary `//` comments and declaration `///` documentation comments.
The token SHALL cover the marker through the byte before the line ending or through end-of-file,
and SHALL carry no semantic attachment.

#### Scenario: Distinguish all line comment forms

- **WHEN** source contains `// note`, `/// declaration`, and `//! module` on separate lines
- **THEN** the lexer emits distinct line-comment, declaration-documentation, and module-documentation token kinds with exact spans
