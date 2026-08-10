## Why

Silk currently recognizes `Effect.map`, `flatMap`, `tap`, `catch`, `retry`, `provide`, and
`provideWith` as privileged semantic operations because ordinary Silk cannot quantify over failure
and requirement rows or execute an Effect while retaining its complete outcome. That duplicates an
Effect library inside elaboration and lowering, makes composition order a compiler concern, and
prevents users from navigating to the implementation of foundational library operations.

## What Changes

- Add finite, kinded generic parameters for failure rows and access-qualified requirement rows,
  including normalization, union, membership, remainder, and subtraction constraints.
- Make closed Effect values ordinary ownership-aware values across parameters, returns, bindings,
  captures, and generic specializations while retaining hidden construction-site identity.
- Define a minimal compiler Effect core: lazy construction, propagating `run`, owned `fail`, an
  effectful operation that reifies a completed `Result<A, E>`, and contravariant adaptation of one
  requirement entry without exposing a runtime requirement record.
- Keep the runner contract abstract and compatible with a future complete-or-suspended execution
  step, while adding no scheduler, fibers, asynchronous operations, or suspension cost now.
- Ship visible ordinary Silk implementations of success, failure, and requirement-channel
  transformations and derive `map`, `mapError`, `mapBoth`, `flatMap`, `tap`, `catch`, `retry`,
  `provide`, and `provideWith` from that core.
- **BREAKING** Remove compiler-special recognition and recipe lowering for standard Effect
  combinators after the source implementations reach evaluator, native, and direct-Wasm parity.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-flow-functions`: Define the compiler-owned Effect core, the covariant success and
  failure channels, the contravariant requirement channel, outcome reification, and
  suspension-compatible execution abstraction.
- `bootstrap-type-generics`: Add finite kinded row parameters and deterministic row constraints for
  generic failure and requirement transformation.
- `bootstrap-callable-values`: Permit ordinary source combinators to accept, retain, run, and return
  hidden-identity Effect values while deriving access from every stored callable and Effect.
- `bootstrap-silk-stdlib`: Ship the canonical visible Effect combinator implementations as ordinary
  Silk modules compiled without library-origin privilege.

## Impact

This changes Effect syntax typing and elaboration, generic substitution and instance discovery,
ownership and layout of Effect parameters, HIR/MIR execution primitives, evaluator and backend
lowering, the intrinsic catalog, standard-library packaging, editor navigation, and Effect tests.
Existing source spelling should remain stable where practical, but compiler-private combinator HIR
and MIR operations are intentionally removed. The change establishes an abstract execution seam for
future suspension without implementing concurrency or changing the current synchronous cost model.
