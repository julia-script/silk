## MODIFIED Requirements

### Requirement: Kernel token vocabulary

The lexer SHALL recognize ASCII whitespace, `//` line comments, `///` documentation comments as a
distinct token kind, the keywords `pub`, `fn`, and `return`, ASCII identifiers, decimal integer
literals, `(`, `)`, `{`, `}`, `:`, `,`, `->`, and end-of-file. An identifier SHALL begin with an
ASCII letter or underscore and continue with ASCII letters, digits, or underscores. A decimal
integer literal SHALL contain one or more ASCII digits.

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
