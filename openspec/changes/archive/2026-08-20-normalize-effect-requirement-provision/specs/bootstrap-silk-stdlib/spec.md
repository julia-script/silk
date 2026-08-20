## ADDED Requirements

### Requirement: Standard provision helpers use canonical key selectors

The canonical Effect source library SHALL expose `provide`, `provideMut`, and `provideEffect` as
ordinary Silk declarations whose explicit selector is `Service` or `Service at Role`. The selector
SHALL NOT contain `&` or `&mut`; each helper's provider parameter SHALL determine available access.
`provideEffect` SHALL acquire a fresh provider for each execution and compose the acquisition
Effect's failure and requirement channels. The superseded `provideWith` name SHALL not resolve as
an alias.

#### Scenario: Resolve only the canonical effectful helper

- **WHEN** tooling inspects the Effect standard-library actor
- **THEN** it exposes `provideEffect` with source spans and does not expose `provideWith`

#### Scenario: Select a non-default role

- **WHEN** a caller supplies `Clock at Primary` to any provision helper
- **THEN** the helper discharges that key and validates provider access separately
