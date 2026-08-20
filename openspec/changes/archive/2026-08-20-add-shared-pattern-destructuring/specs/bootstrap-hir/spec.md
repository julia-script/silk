## ADDED Requirements

### Requirement: HIR carries shared typed statement patterns

HIR SHALL represent unconditional and conditional destructuring with the same typed pattern
selection used by expression matching. Each selection SHALL carry the subject, access, canonical
members, exact selected member, recursive binding paths and types, coverage, irrefutability,
source span, and lexical region. Conditional bodies SHALL remain an acyclic typed region graph.

#### Scenario: Elaborate an irrefutable local pattern

- **WHEN** a nested nominal pattern covers its initializer exactly
- **THEN** HIR contains one total pattern binding with typed recursive field paths

#### Scenario: Elaborate an if-let selection

- **WHEN** one exact union member is conditionally borrowed
- **THEN** HIR contains one selection and two lexical bodies, with bindings visible only in the taken body
