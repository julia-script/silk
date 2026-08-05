## MODIFIED Requirements

### Requirement: Lossless concrete syntax tree

The concrete syntax tree SHALL retain every lexer token exactly once as a leaf in source order,
including whitespace, comments, invalid tokens, and end-of-file. Concrete nodes SHALL expose their
kind, ordered children, and source-owned half-open span. The tree SHALL distinguish the source file,
function declaration, parameter list, parameter declaration, return type, block, return statement,
integer literal expression, identifier expression, call expression, argument list, missing element,
and unexpected-token error region without claiming semantic meaning. The tree SHALL be owned by
the `SyntaxFile` artifact of its source module, and every node and token SHALL be addressable
there by a stable identity.

#### Scenario: Reconstruct parsed source

- **WHEN** all concrete token leaves except end-of-file are sliced from their owning source in tree order
- **THEN** their concatenated bytes reproduce the original source exactly

#### Scenario: Preserve invalid lexer data

- **WHEN** the lexer emits an invalid token inside a parameter or argument list
- **THEN** the concrete tree retains that exact token inside an error region and preserves its lexical diagnostic

#### Scenario: Address tree elements through the artifact

- **WHEN** a source module is parsed into its `SyntaxFile`
- **THEN** every tree node and token leaf resolves to a stable identity qualified by the source identity

### Requirement: Parser diagnostics are deterministic data

Parsing SHALL produce the `SyntaxFile` artifact, which retains the token stream and lexical
diagnostics and exposes parser diagnostics as a separate readonly collection. Every parser
diagnostic SHALL be a unified `Diagnostic` value whose originating phase is the parser, containing
a stable code, severity, concise message, structured reason data, and source-owned primary span.
Within the artifact, parser diagnostics SHALL be ordered by primary span and stable code, and
parsing SHALL return its artifact and diagnostics rather than throwing or failing an Effect for
source mistakes.

#### Scenario: Repeat malformed parsing

- **WHEN** equivalent malformed source files are lexed and parsed repeatedly in fresh processes
- **THEN** their tree kinds, ordered elements, spans, source slices, lexical diagnostics, and parser diagnostics are identical

#### Scenario: Parser diagnostics carry their phase

- **WHEN** a source produces any parser diagnostic
- **THEN** the diagnostic is a unified `Diagnostic` value identifying the parser as its originating phase
