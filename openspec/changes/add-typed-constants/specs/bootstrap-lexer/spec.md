## ADDED Requirements

### Requirement: const is a complete-identifier keyword

The lexer SHALL classify the exact lowercase spelling `const` as a distinct keyword token under the
same longest complete-identifier rule as every other keyword.

#### Scenario: Lex const without consuming prefixes

- **WHEN** source contains `const constant constable`
- **THEN** only `const` is a const-keyword token and the longer spellings remain identifiers
