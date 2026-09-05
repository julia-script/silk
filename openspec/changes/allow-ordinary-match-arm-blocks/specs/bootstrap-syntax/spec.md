## MODIFIED Requirements

### Requirement: Match expressions are lossless in every expression position

Every expression position SHALL accept `match` followed by an optional `move`, `&`, or `&mut` mode,
one scrutinee expression, and a braced source-ordered arm list. Each arm SHALL contain a nominal or
universal pattern, an optional `if` guard expression, `=>`, and either one result expression or an immediate braced ordinary statement block. Newlines and
trivia MAY separate arms without a comma. The concrete tree SHALL retain every token, pattern,
guard, explicit arm body kind, statement, arm boundary, trivia item, and exact span without deciding coverage or types.

#### Scenario: Parse a consuming match initializer

- **WHEN** a binding initializes from `match move event { Token { kind, .. } => kind End {} => 0 }`
- **THEN** the concrete tree retains one match expression with its mode, scrutinee, two ordered arms, patterns, results, and punctuation

#### Scenario: Parse a guarded shared match

- **WHEN** a return expression matches `&event` with a guarded nominal arm followed by `_`
- **THEN** the tree retains the ampersand, guard expression, both fat arrows, and universal identifier in source order

#### Scenario: Parse ordinary blocks in nested expression positions

- **WHEN** matches in an initializer, call argument, and return operand contain empty, sequential, guarded, and nested statement arms alongside expression arms
- **THEN** each remains one match expression with expression-or-block arm bodies, and every block preserves its braces, ordinary statements, comments, and exact token spans

#### Scenario: Keep arm blocks distinct from general expressions

- **WHEN** a brace immediately follows a match arm arrow, while another bare block occurs as an ordinary call argument
- **THEN** only the arm body is accepted as an ordinary statement block; the general expression block remains invalid

## ADDED Requirements

### Requirement: Ordinary match-arm block parsing is bounded and does not imply return

An immediate arm block SHALL use ordinary statement syntax without callable-body implicit-return interpretation. Empty bodies, bindings, writes, explicit `drop`, nested control flow, `run`, `fail`, and transfers SHALL retain their ordinary syntax. Malformed statements or missing braces SHALL recover at a bounded arm, enclosing delimiter, statement, or declaration boundary; recovery SHALL retain original tokens, trivia, exact diagnostic spans, and explicit missing or damaged syntax without swallowing a later arm or declaration.

#### Scenario: Preserve a final expression statement

- **WHEN** an arm body is `{ 42 }` or `{ drop 42 }`
- **THEN** the tree retains an expression statement or explicit drop statement respectively and synthesizes no return statement

#### Scenario: Recover before the following arm

- **WHEN** an ordinary arm block has a malformed statement or missing closing brace before a recognizable later arm
- **THEN** parser-owned diagnostics identify the damaged syntax or insertion span once, and recovery retains the later arm and enclosing match

#### Scenario: Recover before the following declaration

- **WHEN** a match arm block is missing a closing brace before a following top-level declaration
- **THEN** recovery records the missing delimiter at its insertion span and preserves the following declaration as a declaration
