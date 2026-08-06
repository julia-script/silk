## ADDED Requirements

### Requirement: Match tokens use deterministic longest recognition

The lexer SHALL recognize `match` as a complete-identifier keyword and SHALL recognize `&`, `=>`,
and `..` as supported punctuation with exact source spans. Longest recognition SHALL prefer `=>`
over `=`, `..` over `.`, and every existing multi-byte operator over its prefix. The exact spelling
`_` SHALL remain an identifier token and gain universal-pattern meaning only in pattern position.

#### Scenario: Lex a borrowed guarded arm

- **WHEN** source contains `match &value { Token { kind, .. } if guard => kind _ => 0 }`
- **THEN** every keyword, ampersand, brace, dot-dot, guard, fat arrow, identifier, and literal is covered once by the expected token kind

#### Scenario: Preserve keyword and punctuation prefixes

- **WHEN** source contains `matcher = > . .. =>`
- **THEN** `matcher` remains one identifier while the remaining supported and invalid spellings retain deterministic independent coverage
