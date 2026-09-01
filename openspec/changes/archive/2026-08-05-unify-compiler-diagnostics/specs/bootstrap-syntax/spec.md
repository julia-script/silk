## MODIFIED Requirements

### Requirement: Parser diagnostics are deterministic data

The parse result SHALL retain the lexical result and expose parser diagnostics as a separate
readonly collection. Every parser diagnostic SHALL be a unified `Diagnostic` value whose
originating phase is the parser, containing a stable code, severity, concise message, structured
reason data, and source-owned primary span. Within the parse result, parser diagnostics SHALL be
ordered by primary span and stable code, and parsing SHALL return its tree and diagnostics rather
than throwing or failing an Effect for source mistakes.

#### Scenario: Repeat malformed parsing

- **WHEN** equivalent malformed source files are lexed and parsed repeatedly in fresh processes
- **THEN** their tree kinds, ordered elements, spans, source slices, lexical diagnostics, and parser diagnostics are identical

#### Scenario: Parser diagnostics carry their phase

- **WHEN** a source produces any parser diagnostic
- **THEN** the diagnostic is a unified `Diagnostic` value identifying the parser as its originating phase
