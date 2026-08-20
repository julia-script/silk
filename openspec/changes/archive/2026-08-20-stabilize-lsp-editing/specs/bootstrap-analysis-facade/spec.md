## MODIFIED Requirements

### Requirement: Recovery states remain queryable through the facade

Facade snapshot construction SHALL represent every source-induced recovery state as immutable
analysis data rather than leaking an internal exception. Facade results SHALL carry the same
explicit unavailable, missing, ambiguous, and damaged states as the underlying fact tables, and a
damaged module SHALL leave every unrelated declaration fully queryable. A missing or unexpected
recovered syntax child MUST make only its dependent fact unavailable; source shape alone MUST NOT
abort construction of the project revision.

#### Scenario: Query around damage

- **WHEN** one module of a snapshot contains recovered syntax and semantic mistakes
- **THEN** the other module's declarations, functions, and HIR answer completely and the damaged module's facts expose their explicit recovery states

#### Scenario: Analyze an incomplete nominal pattern

- **WHEN** an editor revision ends while a match pattern starts with an incomplete array, reference, parenthesized, callable, or row type
- **THEN** facade snapshot construction completes with parser-owned diagnostics and unavailable dependent pattern facts rather than throwing an internal exception

#### Scenario: Repair source after recovered damage

- **WHEN** a later revision replaces source that previously required recovery with valid source
- **THEN** a new facade snapshot contains the repaired semantic facts without retaining an exceptional state from the earlier revision
