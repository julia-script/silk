## ADDED Requirements

### Requirement: Inspector views expose generalized borrow roots

Inspector ownership views SHALL render parameter, local, pattern, and compiler-owned temporary loan
roots without assuming every non-parameter root is a source binding.

#### Scenario: Inspect a temporary loan

- **WHEN** analysis publishes a loan rooted in an addressable temporary
- **THEN** the inspector labels the temporary deterministically and remains available
