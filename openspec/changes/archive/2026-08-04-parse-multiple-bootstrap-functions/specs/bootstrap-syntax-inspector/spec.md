## ADDED Requirements

### Requirement: Inspect multiple concrete function branches
The Syntax Inspector SHALL provide a two-function preset and display each parsed function as a
separate top-level concrete branch in source order. Until declaration collection is implemented,
the semantic panel SHALL state that it describes only the first function and MUST NOT imply that
later declarations have semantic facts.

#### Scenario: Inspect two parsed functions
- **WHEN** a developer selects the two-function preset
- **THEN** the concrete tree shows two function-declaration branches while the semantic panel clearly identifies its first-function-only boundary

#### Scenario: Inspect recovery at a function boundary
- **WHEN** the first function in a two-function source is missing its closing brace
- **THEN** the tree keeps the missing brace in the first branch and the complete second branch visible
