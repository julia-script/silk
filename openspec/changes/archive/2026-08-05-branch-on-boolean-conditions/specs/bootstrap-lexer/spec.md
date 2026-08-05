## ADDED Requirements

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
