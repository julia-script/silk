## MODIFIED Requirements

### Requirement: Unified diagnostic model

The unary-only deeper-under-application code `SEM0079` SHALL be retired. Zero-argument and
over-arity calls SHALL use the ordinary arity diagnostic, while valid non-empty trailing sections
SHALL produce no arity diagnostic.

#### Scenario: Diagnose only an invalid remaining arity

- **WHEN** a multi-parameter callable receives zero arguments or more arguments than remain
- **THEN** analysis reports the ordinary arity code and never `SEM0079`
