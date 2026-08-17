## ADDED Requirements

### Requirement: Effect row combinators are ordinary fixed-mode Silk source

The standard library SHALL define shared `bindRequirement`, exclusive `bindRequirementMut`, owned
`bindRequirementOwned`, `provide`, `provideMut`, and acquisition-based provision as ordinary Silk
declarations using whole input row `R`, selected row `S`, checked fixed-mode provider constraints,
and `Without<R, S>`. Public wrappers SHALL place `S` first and discharge the same intrinsic wanted
from a definitionally equivalent declared given.

Singleton `Effect.catch` SHALL infer or explicitly accept one nominal `S`, require `S in E`, call
the sealed executable selective primitive, and return `Without<E, S> | F`. Whole-row
recovery SHALL use `Effect.catchAll`; the prior whole-row `catch` alias SHALL not remain. No compiler
phase SHALL recognize these wrappers by standard-library actor, name, or origin.

#### Scenario: Preserve Clock while providing Logger

- **WHEN** `Effect.provideMut` receives an Effect requiring `&mut Clock | &mut Logger` and an exclusive `StdoutLogger` conforming only to `Logger`
- **THEN** ordinary constraint solving selects and removes exactly `&mut Logger`, leaving `&mut Clock`

#### Scenario: Bind all three provider modes ordinarily

- **WHEN** shared, exclusive, and owned wrappers are analyzed
- **THEN** their bodies type-check from declared givens and ordinary capture semantics determine borrow, Copy snapshot, or affine take-once behavior

#### Scenario: Separate singleton and whole-row recovery

- **WHEN** source handles one nominal failure it uses `Effect.catch`; when it handles a reified whole failure row it uses `Effect.catchAll`
- **THEN** the two public contracts remain distinct without compiler-known stdlib dispatch
