## MODIFIED Requirements

### Requirement: The Silk lexer is checked against the canonical lexer

The Silk lexer SHALL be differentially checked against the TypeScript lexer, which remains the
canonical implementation. The corpus SHALL cover whitespace, comments, identifiers, every current
keyword, decimal integer and float forms, single-line and multiline text and byte literals with
valid and malformed escapes, recognized and unknown literal modifiers, physical LF and CRLF,
terminated and unterminated delimiters, every current single and compound punctuation token,
end-of-file, and unsupported byte runs.

#### Scenario: Valid source agrees token by token

- **WHEN** representative valid Silk source is lexed by both implementations
- **THEN** the ordered token kinds and half-open byte spans, including trivia and end of file, are identical

#### Scenario: Invalid source agrees on diagnostics

- **WHEN** source contains unsupported byte runs, unknown literal modifiers, or unterminated literal delimiters
- **THEN** both implementations produce identical token spans and lexical diagnostic identities and spans while applying the committed recovery boundary

#### Scenario: Multiline literal forms agree

- **WHEN** the differential corpus exercises escaped `"""` and `b"""` literals containing quotes, LF, CRLF, indentation, code-like content, and pipeline punctuation
- **THEN** both lexers agree on literal category, complete token boundaries, exact source spans, and following tokens
