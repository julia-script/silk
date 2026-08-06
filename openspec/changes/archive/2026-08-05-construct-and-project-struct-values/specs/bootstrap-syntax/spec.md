## ADDED Requirements

### Requirement: Struct literals parse losslessly

Every expression position SHALL accept a one- or two-segment type path followed by a braced,
comma-separated labeled field-initializer list. Each initializer SHALL retain its field name, colon,
expression, separators, trivia, and exact source span. An empty list SHALL remain valid. Concrete
syntax MUST NOT decide whether the target is a struct, construction is authorized, fields are
complete, or initializer types match.

#### Scenario: Parse a labeled literal

- **WHEN** source contains `Token { kind: kind, lexeme: move lexeme }`
- **THEN** the tree retains the target path, braces, both labeled initializers, comma, nested move expression, and every trivia slice exactly once

#### Scenario: Parse an empty literal

- **WHEN** an expression contains `End {}`
- **THEN** the tree contains one complete zero-field struct-literal branch

### Requirement: Field projections are postfix expressions

Every expression SHALL accept one or more `.field` postfix projections after a primary expression.
Projection SHALL bind more tightly than prefix, infix, equality, and pipeline operations and SHALL
associate left-to-right. The concrete tree SHALL retain each dot and field token independently of
whether the subject is a value or the field resolves.

#### Scenario: Parse a chained projection

- **WHEN** source returns `token.span.start + 1`
- **THEN** the tree nests `token.span` then `.start` before the addition with every token retained

#### Scenario: Distinguish projection from a qualified call

- **WHEN** source contains `Token.make(1)` and `token.kind`
- **THEN** the first remains a qualified call target and the second remains a field-projection expression

### Requirement: Struct-value recovery stays inside the expression

A missing literal target, brace, field name, colon, initializer, comma, projection member, or
projection subject SHALL become explicit recovery data at the nearest expression boundary.
Recovery SHALL preserve later field initializers, arguments, statements, closing braces, and
top-level declarations without interpreting damaged syntax as a complete value.

#### Scenario: Recover a missing field initializer

- **WHEN** a literal contains `Pair { left:, right: 2 }`
- **THEN** `left` retains a missing expression and `right` remains a separate complete initializer

#### Scenario: Recover a missing projection member

- **WHEN** a body contains `value.` before its return boundary
- **THEN** the projection retains a missing field identifier without consuming the following statement or declaration
