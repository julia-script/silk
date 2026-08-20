## ADDED Requirements

### Requirement: Statement pattern bindings are scoped non-shadowing facts

Semantic analysis SHALL publish one shared typed pattern fact for match arms, unconditional local
patterns, and conditional patterns. Each fact SHALL retain its source structure, exact selector,
canonical member evidence, recursive field paths, bindings, access, coverage, irrefutability,
source spans, and causal unavailability. Match-arm and if-let bindings SHALL be visible only in
their selected body; irrefutable let bindings SHALL enter the enclosing block after declaration.
Pattern bindings SHALL obey ordinary non-shadowing and collision rules.

#### Scenario: Publish a local pattern binding

- **WHEN** `let Point { x, .. } = move point` is valid
- **THEN** semantic facts identify `x` as a typed pattern declaration visible after that statement

#### Scenario: Keep mismatch scope empty

- **WHEN** an if-let has an else body
- **THEN** none of the pattern declarations resolve inside the else body
