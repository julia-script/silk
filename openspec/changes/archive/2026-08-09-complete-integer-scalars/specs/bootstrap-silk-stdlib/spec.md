## ADDED Requirements

### Requirement: Option is ordinary canonical Silk source

The standard library SHALL define `Some<T>`, `None`, and transparent `Option<T> = Some<T> | None` in ordinary shipped Silk source. Recoverable integer operations SHALL use this definition without adding an Option-shaped compiler collection primitive.

#### Scenario: Return checked success

- **WHEN** checked integer arithmetic succeeds
- **THEN** it returns the canonical `Some<T>` member containing the exact value

#### Scenario: Return checked failure

- **WHEN** checked integer arithmetic cannot represent a result
- **THEN** it returns canonical `None`
