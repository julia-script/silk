## ADDED Requirements

### Requirement: Logging is canonical visible Silk source

The standard library SHALL ship canonical `.silk` declarations for LogLevel, LogError, Logger,
`Effect.log`, the level-selecting logging operation, the initial stdout and in-memory providers,
and provider-owned recorded observation values where needed.
These declarations SHALL participate in the deterministic standard-library manifest, retain
ordinary source spans in diagnostics and editor facts, and receive no semantic privilege from
their module identity.

#### Scenario: Navigate to the Logger contract

- **WHEN** editor tooling resolves a Logger implementation or `Effect.log` call
- **THEN** go-to-definition opens the canonical shipped Silk declaration rather than a generated TypeScript signature

#### Scenario: Copy the logging contract into user source

- **WHEN** equivalent logging declarations are written in a user module
- **THEN** they receive the same parsing, conformance, ownership, Effect, and lowering behavior without intrinsic registration
