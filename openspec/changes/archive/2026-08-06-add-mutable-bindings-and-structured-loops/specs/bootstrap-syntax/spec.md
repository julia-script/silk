## ADDED Requirements

### Requirement: Mutable bindings and assignments parse losslessly

The parser SHALL recognize `let mut name = expression` and statement-form `place = expression`,
where a place is a binding followed by zero or more field or index projections. The concrete tree
SHALL retain the `mut` token, assignment token, complete place syntax, expression, trivia, and exact
spans. Assignment SHALL remain distinct from equality and SHALL NOT be an expression.

#### Scenario: Parse an indexed field update

- **WHEN** source contains `pairs[index].left = value`
- **THEN** the tree retains the binding root, index projection, field projection, assignment token, and right-hand expression in source order

#### Scenario: Distinguish assignment from equality

- **WHEN** a loop body contains both `current = next` and `current == next`
- **THEN** the first is an assignment statement and the second remains an equality expression

### Requirement: Structured loop syntax is lossless and recoverable

The parser SHALL recognize `while condition { statements }`, bare `break`, and bare `continue` as
statements. Loop bodies SHALL retain ordinary bindings, assignments, conditionals, nested loops,
returns, and control transfers in exact source order. Missing conditions, braces, assignment sides,
or damaged transfers SHALL recover within the nearest statement or loop boundary without consuming a
following statement or declaration.

#### Scenario: Parse nested loops and transfers

- **WHEN** a loop body contains a nested loop with `continue` and then an outer `break`
- **THEN** the concrete tree preserves both loop regions and both transfer statements at their exact spans

#### Scenario: Recover a missing loop brace

- **WHEN** a damaged `while` body reaches the next top-level function declaration
- **THEN** the loop records missing closing syntax without consuming that declaration
