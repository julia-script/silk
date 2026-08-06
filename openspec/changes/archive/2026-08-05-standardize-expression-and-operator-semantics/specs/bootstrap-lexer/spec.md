## ADDED Requirements

### Requirement: Expression operators use deterministic longest tokens

The lexer SHALL recognize `+`, `-`, `*`, `/`, `%`, `!`, `<`, `<=`, `>`, `>=`, `==`, `!=`, and
`|>` as distinct operator tokens with exact source spans. Longest recognition SHALL prefer `->`
over `-`, `<=` over `<`, `>=` over `>`, `==` over `=`, `!=` over `!`, and `|>` over unsupported
prefix fragments. `//` SHALL continue to begin a line comment while a single `/` SHALL be the
division token. Operator recognition SHALL preserve the existing lossless coverage and invalid-byte
recovery guarantees.

#### Scenario: Lex every operator spelling

- **WHEN** source contains the complete operator vocabulary separated by trivia
- **THEN** each spelling produces one supported token with its exact span and source slice

#### Scenario: Prefer comments over division pairs

- **WHEN** source contains `/ // comment` followed by a line ending
- **THEN** the first slash is a division token and the double slash begins one line-comment token

#### Scenario: Distinguish assignment and equality

- **WHEN** source contains `= == ! != < <= > >= |>`
- **THEN** every single- and double-byte spelling is tokenized independently by longest match
