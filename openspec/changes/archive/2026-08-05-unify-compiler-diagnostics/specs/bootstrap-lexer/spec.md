## MODIFIED Requirements

### Requirement: Diagnostics use source-owned byte spans

Every lexical diagnostic SHALL be a unified `Diagnostic` value whose originating phase is the
lexer, containing a stable code, severity, concise message, and primary span owned by the lexed
source file. Within the lexical result, diagnostics SHALL be ordered by primary span and stable
code.

#### Scenario: Order multiple lexical errors

- **WHEN** a source contains invalid byte regions at distinct offsets
- **THEN** the returned diagnostics appear in ascending source order with spans that slice to the exact invalid bytes

#### Scenario: Lexical diagnostics carry their phase

- **WHEN** a source produces any lexical diagnostic
- **THEN** the diagnostic is a unified `Diagnostic` value identifying the lexer as its originating phase
