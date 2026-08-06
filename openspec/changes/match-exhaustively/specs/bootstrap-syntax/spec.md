## ADDED Requirements

### Requirement: Match expressions are lossless in every expression position

Every expression position SHALL accept `match` followed by an optional `move`, `&`, or `&mut` mode,
one scrutinee expression, and a braced source-ordered arm list. Each arm SHALL contain a nominal or
universal pattern, an optional `if` guard expression, `=>`, and one result expression. Newlines and
trivia MAY separate arms without a comma. The concrete tree SHALL retain every token, pattern,
guard, arm boundary, trivia item, and exact span without deciding coverage or types.

#### Scenario: Parse a consuming match initializer

- **WHEN** a binding initializes from `match move event { Token { kind, .. } => kind End {} => 0 }`
- **THEN** the concrete tree retains one match expression with its mode, scrutinee, two ordered arms, patterns, results, and punctuation

#### Scenario: Parse a guarded shared match

- **WHEN** a return expression matches `&event` with a guarded nominal arm followed by `_`
- **THEN** the tree retains the ampersand, guard expression, both fat arrows, and universal identifier in source order

### Requirement: Nominal patterns are lossless and recursively recoverable

A nominal pattern SHALL retain its one- or two-segment type path, braces, ordered field patterns,
commas, nested nominal patterns, shorthand bindings, explicit local names, and optional `..` marker.
Missing type names, fields, colons, nested patterns, commas, or braces SHALL remain explicit recovery
data bounded by the next field, guard, fat arrow, arm, closing brace, statement, or declaration.

#### Scenario: Parse a nested renamed binding

- **WHEN** a pattern spells `Token { span: Span { start: offset, .. }, .. }`
- **THEN** the tree retains both nominal patterns, both omission markers, and the renamed leaf binding exactly

#### Scenario: Recover a damaged arm locally

- **WHEN** one pattern or guard is damaged before its fat arrow
- **THEN** its recovery nodes and diagnostics remain inside that arm while the following arm and enclosing expression remain parseable
