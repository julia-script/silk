## ADDED Requirements

### Requirement: Inherent impl declarations have one canonical layout

Formatting SHALL render an inherent impl as `impl[<Binders>] Owner[<Binders>] {` followed by its
members using the ordinary function-declaration layout with the same member separation the
conformance layout uses, and a closing brace on its own line. Doc comments and ordinary comments attached to the head or to a
member SHALL keep their attachment. Formatting SHALL be idempotent and MUST NOT rewrite an inherent
impl into a conformance or vice versa.

#### Scenario: Format a generic inherent impl

- **WHEN** the formatter processes `impl<T>   Option<T>{pub fn none()->Self{return Option<T>.None}}`
- **THEN** the output is the canonical multi-line layout and formatting the output again is unchanged

#### Scenario: Preserve member documentation

- **WHEN** a member inside an inherent impl carries a `///` doc block
- **THEN** the formatted output keeps the block immediately above that member
