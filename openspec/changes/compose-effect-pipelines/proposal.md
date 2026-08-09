## Why

Valid Silk effect pipelines fail when an effect transformation such as `Effect.map` protects a
service-provision recipe and is run directly from an effectful entrypoint. The compiler either
loses the generated entry runner or, when the expression is stored first, emits an unavailable-
transform trap, contradicting the language's existing promise that piped and data-first Effect
composition are equivalent.

## What Changes

- Lower nested Effect recipes compositionally so transformations, service provision, recovery,
  retry, and acquisition may protect one another in any semantically valid order.
- Preserve callable environments, provider loans, failure/requirement rows, cleanup, and execution
  order across direct, grouped, and stored pipeline forms.
- Reject invalid compositions during semantic analysis instead of emitting compiler fallback traps
  or throwing implementation `RangeError`s.
- Add a public-source pipeline pressure matrix spanning pure and effectful entrypoints, ordinary
  values, affine values, `map`, `flatMap`, `tap`, `catch`, `retry`, `provide`, and `provideWith`.
- Rewrite the lexer and other representative pressure-program entrypoints into pipeline form where
  the data/effect flow remains clearer than imperative staging.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-flow-functions`: Require valid Effect operations to compose through direct, grouped,
  and stored pipelines, including transformations around service provision.
- `bootstrap-mir`: Require composed Effect recipes to lower without unavailable-transform traps or
  missing generated runners.
- `bootstrap-compiler-driver`: Extend differential acceptance with a broad source-level pipeline
  matrix and the lexer pressure composition.

## Impact

The change primarily affects Effect recipe lowering in `packages/compiler/src/Lower.ts`, MIR and
backend/evaluator acceptance tests, and representative Silk sources under `examples/`. It adds no
new syntax, runtime scheduler, public intrinsic, or compatibility layer.
