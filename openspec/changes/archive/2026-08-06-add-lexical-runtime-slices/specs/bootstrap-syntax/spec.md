## ADDED Requirements

### Requirement: Lexical slice syntax is lossless and recoverable

The parser SHALL recognize shared `&[T]` and exclusive `&mut [T]` type branches and prefix `&` and
`&mut` borrow-expression branches. It SHALL retain every ampersand, keyword, bracket, nested element
type, trivia token, recovery element, and exact source-owned span without deciding whether the
operand is borrowable or the type is permitted at that source position.

#### Scenario: Parse a shared slice parameter and borrow argument

- **WHEN** source spells `fn fold(values: &[I32]) -> I32 { return use(&values) }`
- **THEN** the tree retains one shared slice type and one shared borrow expression with their punctuation and provenance in source order

#### Scenario: Parse an exclusive slice parameter and borrow argument

- **WHEN** source spells `fn edit(values: &mut [I32]) -> I32 { return use(&mut values) }`
- **THEN** the tree retains both `mut` keywords under distinct exclusive slice-type and borrow-expression branches

#### Scenario: Recover a damaged slice type

- **WHEN** a parameter starts a slice type but omits its element or closing bracket before the parameter boundary
- **THEN** the parser inserts explicit missing syntax, preserves following parameters and the function body, and emits deterministic parser diagnostics
