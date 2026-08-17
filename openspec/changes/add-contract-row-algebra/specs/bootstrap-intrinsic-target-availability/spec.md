## ADDED Requirements

### Requirement: Selective catch is executable on every supported target

`Intrinsic.catchFailure` SHALL belong to the ordinary executable intrinsic inventory for the
evaluator, WebAssembly, and native targets. A valid direct or wrapped selective-catch dependency
SHALL pass target availability and reach the same specialized MIR semantics on every supported
target. No catch-specific `AnalysisOnly` state or `SEM0098` availability diagnostic SHALL remain.

Target availability SHALL remain derived from the canonical intrinsic operation rather than from
standard-library wrapper spelling. Invalid syntax, kind, inference, or constraint diagnostics SHALL
prevent invalid calls from reaching target selection or lowering.

#### Scenario: Admit a reachable wrapper on every target

- **WHEN** a reachable ordinary `Effect.catch` wrapper expands to `Intrinsic.catchFailure`
- **THEN** evaluator, WebAssembly, and native target selection all admit the dependency

#### Scenario: Admit the direct intrinsic on every target

- **WHEN** a valid reachable call names `Intrinsic.catchFailure` directly
- **THEN** it has the same executable target set as the ordinary wrapper

#### Scenario: Reject invalid calls before target selection

- **WHEN** selective catch has invalid syntax, generic kinds, inference, or singleton membership
- **THEN** the corresponding semantic diagnostic is reported and no catch-specific availability diagnostic is emitted
