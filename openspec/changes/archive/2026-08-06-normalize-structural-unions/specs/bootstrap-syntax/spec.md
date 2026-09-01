## ADDED Requirements

### Requirement: Structural union type syntax is lossless and recoverable

The parser SHALL recognize `|` as a type-level union separator in every supported type position.
Union parsing SHALL preserve every member, separator, parenthesis, qualification, trivia item, and
exact span in source order while allowing semantic normalization to erase order, duplicates, and
nesting later. Type-level `|` SHALL remain distinct from expression operators.

#### Scenario: Parse a nested qualified union

- **WHEN** a return type contains `syntax.Token | (syntax.End | syntax.Token)`
- **THEN** the concrete tree retains all three member spellings, both separators, parentheses, and exact spans

#### Scenario: Recover a missing union member

- **WHEN** a parameter type contains `Token |` before its closing delimiter
- **THEN** the union records an explicit missing member without consuming the following parameter or function body

### Requirement: Never type syntax is explicit

The parser SHALL recognize `Never` as a type reference wherever a declared type is accepted and
SHALL retain its exact token and span. `Never` SHALL have no literal or constructor syntax.

#### Scenario: Parse Never in a return type

- **WHEN** a function declares `Never` as its return type
- **THEN** the concrete tree retains one complete declared-type node for the built-in type
