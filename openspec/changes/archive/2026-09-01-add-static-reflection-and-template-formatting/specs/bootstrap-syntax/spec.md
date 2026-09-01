## ADDED Requirements

### Requirement: Static iteration syntax is lossless and recoverable

The parser SHALL recognize `static for <binding> in <expression> { <statements> }` exactly as a
statement form. The syntax tree SHALL retain the `static`, `for`, and `in` keywords, binding,
iterable expression, body, trivia, delimiters, and source spans without deciding whether the
iterable is static, finite, homogeneous, heterogeneous, or reflectable. The form MUST NOT parse in a
declaration list or create a declaration node.

#### Scenario: Parse one static field iteration

- **WHEN** source contains `static for field in Reflect.fields<Args>() { display(field) }`
- **THEN** syntax retains one static-for statement with its complete iterable call and body in concrete order

### Requirement: Static iteration recovery remains locally bounded

A missing iteration binding, `in` keyword, iterable expression, opening brace, body, or closing
brace SHALL remain explicit missing syntax under the existing bounded recovery rules. Recovery
MUST preserve the following statement, enclosing block boundary, and following declaration.

#### Scenario: Recover a missing iterable

- **WHEN** `static for field in` is followed by a block and then a return statement
- **THEN** the static-for node records a missing iterable, retains its block, and preserves the following return
