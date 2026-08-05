## MODIFIED Requirements

### Requirement: Kernel token vocabulary

The lexer SHALL recognize ASCII whitespace, `//` line comments, `///` documentation comments as a
distinct token kind, the keywords `pub`, `fn`, `return`, and the provisional `import`, ASCII
identifiers, decimal integer literals, `(`, `)`, `{`, `}`, `:`, `,`, `->`, and end-of-file. An
identifier SHALL begin with an ASCII letter or underscore and continue with ASCII letters,
digits, or underscores. A decimal integer literal SHALL contain one or more ASCII digits.

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
