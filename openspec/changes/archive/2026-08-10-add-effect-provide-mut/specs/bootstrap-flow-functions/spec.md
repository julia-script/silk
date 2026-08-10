## MODIFIED Requirements

### Requirement: Provision distinguishes shared borrow, exclusive borrow, and acquisition

`Effect.provide` SHALL capture an existing provider through a shared borrow and MUST reject an
Effect whose selected capability-role requirement needs exclusive access. `Effect.provideMut`
SHALL capture an existing provider through an exclusive scoped borrow and SHALL accept any provider
type that conforms to the selected service capability. Neither borrowed operation SHALL imply
provider ownership or per-run cleanup. `Effect.provideWith` SHALL acquire a fresh affine provider
owner per execution and drop every successfully acquired owner after success or typed failure
without replacing the original outcome.

#### Scenario: Provide one shared service

- **WHEN** an Effect with one shared service requirement is composed with `Effect.provide(&service)`
- **THEN** the resulting Effect borrows that provider, removes the selected requirement, and preserves its success, failure, and remaining requirement channels

#### Scenario: Provide one exclusive service implementation

- **WHEN** an Effect requiring exclusive access to capability `C` is composed with `Effect.provideMut(&mut provider)` and the provider type conforms to `C`
- **THEN** the resulting Effect borrows the provider exclusively, removes that capability-role requirement, and writes provider mutations back before the borrow ends

#### Scenario: Catch outside per-run acquisition

- **WHEN** a failing Effect is wrapped by `provideWith` and then by `Effect.catch`
- **THEN** the per-run provider drops before recovery begins

### Requirement: Standard Effect combinators are library-defined

`map`, `mapError`, `mapBoth`, `flatMap`, `tap`, `catch`, `retry`, `provide`, `provideMut`, and
`provideWith` SHALL resolve to canonical ordinary Silk declarations. The compiler MUST NOT select
their semantics from their names, actors, library origin, or a dedicated combinator HIR/MIR
operation. Equivalent user code using the compiler-owned Effect core SHALL receive the same typing,
ownership, execution, and cleanup behavior.

#### Scenario: Navigate and compile map as Silk

- **WHEN** a program calls or navigates to `Effect.map`
- **THEN** the target is canonical shipped Silk source compiled through ordinary declaration, callable, ownership, specialization, and lowering paths

#### Scenario: Navigate and compile exclusive provision as Silk

- **WHEN** a program calls either the data-first or piped form of `Effect.provideMut`
- **THEN** the target is canonical shipped Silk source and both forms produce the same contract and execution behavior

#### Scenario: Define an equivalent user combinator

- **WHEN** user source defines the same generic success-channel transformation under another name
- **THEN** it compiles and executes without intrinsic registration or a compiler-recognized operation identity
