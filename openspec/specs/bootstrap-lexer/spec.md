# bootstrap-lexer Specification

## Purpose

Turn exact Silk source bytes into a deterministic, lossless token stream for the first parser
slice while retaining trivia and recoverable lexical errors.
## Requirements
### Requirement: Kernel token vocabulary

The lexer SHALL recognize ASCII whitespace, `//` line comments, `///` documentation comments as a
distinct token kind, the keywords `pub`, `fn`, `return`, `let`, `move`, and the provisional
`import`, ASCII identifiers, decimal integer literals, `(`, `)`, `{`, `}`, `:`, `,`, `=`, `.`,
`-`, `->`, and end-of-file. An identifier SHALL begin with an ASCII letter or underscore and
continue with ASCII letters, digits, or underscores. A decimal integer literal SHALL contain one
or more ASCII digits. A `-` immediately followed by `>` SHALL remain one arrow token; any other
`-` SHALL be one minus token.

#### Scenario: Lex the first parser fixture

- **WHEN** the source bytes spell `pub fn main() -> I32 { return 42 }`
- **THEN** the token stream contains the expected keywords, identifiers, punctuation, integer literal, trivia, and end-of-file in source order

#### Scenario: Lex a typed parameter and call argument

- **WHEN** the source bytes spell `pub fn identity(value: I32) -> I32 { return value }` followed by a call `identity(42)`
- **THEN** the colon and all list punctuation are distinct supported tokens with exact source spans

#### Scenario: Preserve a keyword prefix

- **WHEN** the source bytes spell `public function returnValue`
- **THEN** all three words are identifier tokens rather than keyword tokens followed by suffixes

#### Scenario: Lex a documentation comment

- **WHEN** the source bytes spell `/// doc` on its own line before a function
- **THEN** the stream contains one documentation-comment token distinct from the line-comment kind, covering the slashes through the byte before the line ending

#### Scenario: Lex the import keyword

- **WHEN** the source bytes spell `import math` followed by `importer`
- **THEN** the stream contains one import-keyword token, an identifier `math`, and an identifier `importer` rather than a keyword prefix

#### Scenario: Lex a binding statement

- **WHEN** the source bytes spell `let answer = 42` followed by `letter movement`
- **THEN** the stream contains one let-keyword token, an identifier, one equals token, and an integer literal, while `letter` and `movement` remain identifier tokens rather than keyword prefixes

#### Scenario: Distinguish equals from the arrow

- **WHEN** the source bytes spell `= ->` separated by a space
- **THEN** the stream contains one equals token and one arrow token, each with its exact span

#### Scenario: Lex a signed literal and a qualified callee

- **WHEN** the source bytes spell `-42 I32.add`
- **THEN** the stream contains one minus token, one integer literal, one identifier, one dot token, and one identifier, each with its exact span

#### Scenario: Distinguish minus from the arrow

- **WHEN** the source bytes spell `- -> -5`
- **THEN** the stream contains one minus token, one arrow token, and a minus token followed by an integer literal

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
end-of-file. A comment beginning with exactly `///` SHALL be a documentation-comment token with
the same coverage rule and SHALL NOT carry semantic attachment. Line endings following comments
SHALL remain separate whitespace tokens.

#### Scenario: Lex a line comment

- **WHEN** a line comment is followed by a line ending and another token
- **THEN** the stream contains a comment token, a whitespace token containing the exact line ending, and the following token

#### Scenario: Lex a final line comment

- **WHEN** a line comment reaches end-of-file without a line ending
- **THEN** the comment token covers every remaining byte and is followed by the end-of-file token

#### Scenario: Distinguish documentation from plain comments

- **WHEN** a source contains both `// note` and `/// doc` comments
- **THEN** each is emitted with its own token kind and both remain trivia with exact source spans

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

### Requirement: Conditional keywords join the vocabulary

The lexer SHALL recognize `if`, `else`, `true`, and `false` as keyword tokens under the same
complete-identifier rule as every other keyword: a longer identifier beginning with a keyword
spelling SHALL remain one identifier token.

#### Scenario: Lex a conditional statement

- **WHEN** the source bytes spell `if flag { return true } else { return false }`
- **THEN** the stream contains if-keyword, identifier, braces, return-keyword, true-keyword, else-keyword, and false-keyword tokens with exact spans

#### Scenario: Preserve conditional keyword prefixes

- **WHEN** the source bytes spell `iffy elsewhere truer falsehood`
- **THEN** all four words are identifier tokens

