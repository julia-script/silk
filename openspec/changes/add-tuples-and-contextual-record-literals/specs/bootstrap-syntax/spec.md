## ADDED Requirements

### Requirement: Tuple and contextual record syntax is lossless and recoverable

The lexer and parser SHALL reserve `tuple` and parse `tuple Name(T0, T1, ...)` as a declaration with
ordered explicit element types. Expression grammar SHALL distinguish parenthesized expressions,
unit, positional tuple literals, named tuple construction calls, positional projections such as
`.0`, and record literals beginning with `.{`. Record literal members SHALL contain an identifier,
colon, and expression. Tuple and record lists SHALL preserve commas, optional trailing commas,
trivia, and exact source spans.

Missing tuple names, element types, elements, record labels, colons, values, commas, or closing
delimiters SHALL use the existing explicit-missing syntax and bounded expression or declaration
recovery. A colon inside a positional tuple literal SHALL NOT create labeled-tuple syntax. The
leading dot on a record literal SHALL keep it distinct from a block in every expression position.

#### Scenario: Parse the aggregate forms distinctly

- **WHEN** source contains a tuple declaration, named construction, positional literal, positional projection, record literal, grouped expression, and unit
- **THEN** syntax retains seven distinct source forms with every token, delimiter, trivia slice, and span in source order

#### Scenario: Recover a damaged record literal

- **WHEN** one record member omits its colon or value before a following member and statement
- **THEN** recovery records the missing syntax, preserves the following member and statement, and terminates without a declaration-level cascade

#### Scenario: Reject labeled tuple syntax

- **WHEN** a positional tuple literal contains `name: value`
- **THEN** syntax reports the unexpected colon without reinterpreting the literal as a record or block
