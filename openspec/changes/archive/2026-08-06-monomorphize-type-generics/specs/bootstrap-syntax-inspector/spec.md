## ADDED Requirements

### Requirement: Generic inspection remains unified in labs

The existing `/labs` workbench SHALL present generic syntax, semantic substitutions, instances,
ownership, layouts, MIR, evaluation, and backend artifacts through coordinated selection and shared
diagnostics. It MUST NOT introduce a standalone generic inspector or recompute specialization in the
browser.

#### Scenario: Follow one generic call

- **WHEN** a user selects an explicit or inferred generic call in `/labs`
- **THEN** every available pane coordinates on its canonical specialization and originating source span
