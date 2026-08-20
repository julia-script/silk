## ADDED Requirements

### Requirement: Shared pattern positions are lossless and recursively recoverable

The parser SHALL build one lossless recursive pattern grammar for match arms, unconditional local
bindings, and statement-form conditional bindings. The grammar SHALL retain exact type selectors,
whole-value bindings, field shorthand, field renaming, nested nominal destructuring, rest markers,
wildcards, access-bearing initializer expressions, separators, trivia, and recovery tokens without
giving patterns executable call semantics.

#### Scenario: Parse one recursive local pattern

- **WHEN** source writes `let Pair { point: Point { x, .. }, extra } = move pair`
- **THEN** the syntax tree retains one nested pattern tree and the complete initializer expression

#### Scenario: Parse statement-form if-let

- **WHEN** source writes `if let i32 number = &value { use(number) } else { fallback() }`
- **THEN** the syntax tree retains the pattern, initializer, taken body, and optional mismatch body
