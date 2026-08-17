## ADDED Requirements

### Requirement: Effect documentation teaches exact contract-row transformation

Generated and authored standard-library documentation SHALL explain failure rows as nominal sets,
requirement rows as capability-role keyed access-labelled rows, union access joining, exact
membership/difference, and forward-only `Without<R, S>` including set-to-set removal. It SHALL
distinguish provider compatibility from stored-member identity and explain why a stronger provider
still subtracts the exact stored access.

Effect reference pages SHALL document selected-row-first generic calls, shared/exclusive/owned
binding, conformance-based provider matches, ambiguity diagnostics, partial application, acquisition
cleanup, singleton `catch`, and executable whole-row `catchAll`. Examples SHALL compile or carry an
explicit analysis-only diagnostic expectation.

#### Scenario: Document the Logger and Clock regression

- **WHEN** readers view `Effect.provideMut`
- **THEN** an example shows `StdoutLogger` removing `&mut Logger` from `&mut Clock | &mut Logger` while preserving `Clock`

#### Scenario: Document set-to-set difference

- **WHEN** readers view contract-row algebra
- **THEN** examples cover singleton, absent-member no-op, exact access mismatch, and set-to-set `Without`

#### Scenario: Document catch availability honestly

- **WHEN** readers view selective `Effect.catch`
- **THEN** the reference explains its singleton typing contract, current `SEM0098` analysis-only availability, and use of `catchAll` for executable whole-row recovery
