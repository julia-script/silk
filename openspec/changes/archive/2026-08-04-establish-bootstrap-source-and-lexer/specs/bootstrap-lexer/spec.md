## Purpose

Turn exact Silk source bytes into a deterministic, lossless token stream for the first parser
slice while retaining trivia and recoverable lexical errors.

## ADDED Requirements

### Requirement: Kernel token vocabulary
The lexer SHALL recognize ASCII whitespace, `//` line comments, the keywords `pub`, `fn`, and
`return`, ASCII identifiers, decimal integer literals, `(`, `)`, `{`, `}`, `->`, and end-of-file.
An identifier SHALL begin with an ASCII letter or underscore and continue with ASCII letters,
digits, or underscores. A decimal integer literal SHALL contain one or more ASCII digits.

#### Scenario: Lex the first parser fixture
- **WHEN** the source bytes spell `pub fn main() -> I32 { return 42 }`
- **THEN** the token stream contains the expected keywords, identifiers, punctuation, integer literal, trivia, and end-of-file in source order

#### Scenario: Preserve a keyword prefix
- **WHEN** the source bytes spell `public function returnValue`
- **THEN** all three words are identifier tokens rather than keyword tokens followed by suffixes

### Requirement: Lossless token coverage
Every non-end-of-file token SHALL own a non-empty span, token spans SHALL be contiguous and
non-overlapping in source order, and their concatenated source slices SHALL reconstruct every input
byte exactly once. The end-of-file token SHALL own the empty span at the source length.

#### Scenario: Reconstruct trivia-heavy source
- **WHEN** a source contains spaces, tabs, line endings, comments, supported tokens, and invalid bytes
- **THEN** concatenating every non-end-of-file token slice reproduces the original bytes exactly

### Requirement: Trivia remains explicit
The lexer SHALL emit contiguous supported whitespace as whitespace tokens and SHALL emit each `//`
line comment from its opening slashes through the byte before its line ending or through
end-of-file. Line endings following comments SHALL remain separate whitespace tokens.

#### Scenario: Lex a line comment
- **WHEN** a line comment is followed by a line ending and another token
- **THEN** the stream contains a comment token, a whitespace token containing the exact line ending, and the following token

#### Scenario: Lex a final line comment
- **WHEN** a line comment reaches end-of-file without a line ending
- **THEN** the comment token covers every remaining byte and is followed by the end-of-file token

### Requirement: Deterministic longest token recognition
The lexer SHALL choose the longest supported token beginning at the current byte, classify a
complete identifier as a keyword only when its full bytes equal a reserved keyword, and preserve
each token's exact source span. Tokenization MUST NOT depend on locale, Unicode normalization,
object identity, or process state.

#### Scenario: Recognize the arrow token
- **WHEN** the next bytes are `->`
- **THEN** the lexer emits one arrow token spanning both bytes

#### Scenario: Repeat lexing
- **WHEN** equivalent source files are lexed repeatedly in fresh processes
- **THEN** their token kinds, spans, source slices, and lexical diagnostics are identical

### Requirement: Invalid bytes remain recoverable data
Bytes that cannot begin any supported token SHALL be emitted as invalid tokens and SHALL produce a
stable lexical diagnostic covering the same span. The lexer SHALL consume at least one byte,
continue at the next supported token boundary, and return the complete token stream and diagnostic
collection rather than throwing or failing an Effect.

#### Scenario: Recover after an invalid byte
- **WHEN** an unsupported byte appears between two identifiers
- **THEN** the lexer emits the first identifier, an invalid token and diagnostic, the second identifier, and end-of-file

#### Scenario: Preserve unsupported non-ASCII bytes
- **WHEN** the source contains a multi-byte UTF-8 sequence outside the kernel vocabulary
- **THEN** every byte remains covered by invalid token data and the lexer continues after the unsupported sequence

### Requirement: Diagnostics use source-owned byte spans
Every lexical diagnostic SHALL contain a stable code, concise message, and primary span owned by
the lexed source file. Diagnostics SHALL be ordered by primary span and stable code.

#### Scenario: Order multiple lexical errors
- **WHEN** a source contains invalid byte regions at distinct offsets
- **THEN** the returned diagnostics appear in ascending source order with spans that slice to the exact invalid bytes
