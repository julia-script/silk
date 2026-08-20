## ADDED Requirements

### Requirement: Pattern-local hover uses source-like declarations

Hover over a shared pattern declaration or reference SHALL use its compiler-published identity and
render the source-like local binding name and exact narrowed type. Hover MUST NOT infer a type by
reparsing the pattern or inspecting runtime tags.

#### Scenario: Hover a pattern local

- **WHEN** hover selects a reference introduced by `let Point { x, .. } = point`
- **THEN** it renders `let x: i32` from the semantic pattern fact
