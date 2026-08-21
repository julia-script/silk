## MODIFIED Requirements

### Requirement: Effect combinators are canonical visible Silk source

The standard library SHALL ship the Effect operation module under canonical identity `silk/effect`
and SHALL ship canonical `.silk` declarations for success, failure, and requirement-channel
transformations and for the derived `map`, `mapError`, `mapBoth`, `flatMap`, `tap`, `catch`, `retry`,
`provide`, and `provideEffect` API. The prior identity `silk/effects` MUST NOT resolve as an alias,
fallback, or second module. These files SHALL be the only editable source of truth, participate in
the deterministic standard-library manifest, and retain ordinary source spans in semantic facts,
diagnostics, documentation, hover, and navigation.

#### Scenario: Import the canonical Effect module

- **WHEN** source imports `silk.effect as Effect`
- **THEN** ordinary module resolution loads canonical module `silk/effect` and qualified operations resolve to its source declarations

#### Scenario: Reject the removed plural identity

- **WHEN** source imports `silk.effects as Effect`
- **THEN** ordinary module resolution reports that `silk/effects` is unavailable

#### Scenario: Navigate to a standard Effect combinator

- **WHEN** editor tooling resolves a call to `Effect.mapBoth`
- **THEN** go-to-definition opens the canonical shipped Silk declaration rather than a generated TypeScript signature or embedded string

#### Scenario: Diagnose standard Effect source normally

- **WHEN** an Effect library body violates row, callable, or ownership rules
- **THEN** the compiler reports the same source diagnostic an equivalent user declaration receives
