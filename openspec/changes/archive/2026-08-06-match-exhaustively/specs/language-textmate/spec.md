## ADDED Requirements

### Requirement: TextMate grammars cover exhaustive-match syntax

The Silk TextMate grammar and generated VS Code grammar SHALL assign consistent scopes to the
`match` keyword, access-mode punctuation, nominal and universal patterns, guards, fat arrows,
omission markers, bindings, and arm expressions. Keyword parity tests SHALL continue to compare the
grammar vocabulary with compiler token definitions.

#### Scenario: Scope one consuming match

- **WHEN** a TextMate consumer tokenizes a consuming two-arm nominal match
- **THEN** `match`, `move`, type names, bindings, `..`, `=>`, and `_` receive stable appropriate scopes without changing pipeline or union-token recognition
