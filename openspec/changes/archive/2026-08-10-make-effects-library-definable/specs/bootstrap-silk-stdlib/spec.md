## ADDED Requirements

### Requirement: Effect combinators are canonical visible Silk source

The standard library SHALL ship canonical `.silk` declarations for success, failure, and
requirement-channel transformations and for the derived `map`, `mapError`, `mapBoth`, `flatMap`,
`tap`, `catch`, `retry`, `provide`, and `provideWith` API. These files SHALL be the only editable
source of truth, participate in the deterministic standard-library manifest, and retain ordinary
source spans in semantic facts, diagnostics, documentation, hover, and navigation.

#### Scenario: Navigate to a standard Effect combinator

- **WHEN** editor tooling resolves a call to `Effect.mapBoth`
- **THEN** go-to-definition opens the canonical shipped Silk declaration rather than a generated TypeScript signature or embedded string

#### Scenario: Diagnose standard Effect source normally

- **WHEN** an Effect library body violates row, callable, or ownership rules
- **THEN** the compiler reports the same source diagnostic an equivalent user declaration receives

### Requirement: Effect library sources have no semantic privilege

The compiler SHALL recognize only the closed low-level Effect operations documented by the flow-
function contract. It MUST NOT branch on the `Effect` namespace, combinator declaration identity,
standard-library module identity, or source location when analyzing or lowering derived
combinators. Temporary differential implementations MAY coexist during migration but MUST be
removed before this change is complete.

#### Scenario: Rename a user-defined equivalent

- **WHEN** an equivalent generic combinator is copied into user source under another legal name
- **THEN** it receives the same available HIR, ownership facts, MIR, and execution behavior as the standard declaration
