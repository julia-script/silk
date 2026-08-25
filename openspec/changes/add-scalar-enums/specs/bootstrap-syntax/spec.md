## ADDED Requirements

### Requirement: Scalar enum declarations are lossless and recoverable

The parser SHALL retain a scalar enum declaration's optional visibility, `enum` keyword, optional
parenthesized representation type, name, braces, ordered members, commas, trivia, and exact spans.
Each member SHALL retain its name and optional equals plus signed decimal integer literal without
assigning or validating a discriminant. Missing or unexpected representation and member elements
SHALL use explicit missing or error syntax, and recovery SHALL resume at the next comma, closing
brace, following declaration, or end-of-file.

#### Scenario: Parse default and represented enums

- **WHEN** source contains one default enum and one `enum(u8)` with explicit discriminants
- **THEN** both declarations retain all tokens and members in source order without semantic width decisions

#### Scenario: Recover a damaged member

- **WHEN** one member contains an unexpected token before a later comma-bounded member
- **THEN** the damage remains in an error region and the later member plus following declaration remain parseable

### Requirement: Qualified enum member paths remain source-faithful expressions and patterns

The syntax tree SHALL retain `EnumName.Member` with both identifiers and the dot in expression and
match-pattern positions. Syntax SHALL NOT decide whether the qualifier is an enum, whether the member
exists, or whether it belongs to the scrutinee type.

#### Scenario: Parse one enum member match arm

- **WHEN** a match arm pattern spells `AssertionResult.Pass`
- **THEN** syntax retains the complete qualified path and exact span without treating it as an integer pattern
