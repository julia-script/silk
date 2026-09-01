## MODIFIED Requirements

### Requirement: Kernel token vocabulary

The lexer SHALL recognize ASCII whitespace, `//` line comments, the keywords `pub`, `fn`, and
`return`, ASCII identifiers, decimal integer literals, `(`, `)`, `{`, `}`, `:`, `,`, `->`, and
end-of-file. An identifier SHALL begin with an ASCII letter or underscore and continue with ASCII
letters, digits, or underscores. A decimal integer literal SHALL contain one or more ASCII digits.

#### Scenario: Lex the first parser fixture

- **WHEN** the source bytes spell `pub fn main() -> I32 { return 42 }`
- **THEN** the token stream contains the expected keywords, identifiers, punctuation, integer literal, trivia, and end-of-file in source order

#### Scenario: Lex a typed parameter and call argument

- **WHEN** the source bytes spell `pub fn identity(value: I32) -> I32 { return value }` followed by a call `identity(42)`
- **THEN** the colon and all list punctuation are distinct supported tokens with exact source spans

#### Scenario: Preserve a keyword prefix

- **WHEN** the source bytes spell `public function returnValue`
- **THEN** all three words are identifier tokens rather than keyword tokens followed by suffixes
