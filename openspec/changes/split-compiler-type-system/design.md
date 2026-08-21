## Context

See proposal.md. Each split follows a seam already present in the code; the phase entry points (`instances`, `discover`, `frontend`, `realize`, `prepare`) stay put and callers are unchanged.

## Decisions

- **`Type.ts` → `TypeInference.ts`**: move `GenericArgumentConflict` through `substitution` (bind, infer, row/failure/requirement argument inference, `prefixSubstitution`) as one self-contained actor that only reads `Type` predicates. `Type.ts` keeps the vocabulary, keys/encoding, row policies, and re-exports `infer`/`inferOpenGenericArguments`.
- **`Instances.ts` split**: keep `discover`/`resolveEntry`/`specialize`/`keyOf`/`concreteConstraintEvidence` in `Instances`; move `copyDropViolations`/`requirementBindingViolations`/`unlowerableWitnessViolations`/`storedExecutableViolations`/`violationDiagnostics` to `InstanceDiagnostics.ts`; move `callableOriginOf`/`effectOriginOf`/`resultEffectIdentity`/`concreteCallables`/`concreteEffects`/`suspensionGraph`/`suspendableNodes`/`reachableIntrinsics`/`interfaceWitnessTargets`/`callTargets` to `ExecutableOrigin.ts`.
- **`Pipeline.ts` split**: `realize` (Analysis-facing) and `prepare` (Driver-facing) share one `discoverAndLower(self, options)` returning `{ instances, diagnostics, target, layout, mir, report }`; each public entry maps that result through its own gates. `frontend` and `frontendProject` move to their own modules with the reuse machinery (`ProjectReuseBasis`, `checkpointModuleBatch`).

## Risks / Trade-offs

- [Huge code motion] → do each actor extraction as its own commit; run `pnpm typecheck`/`pnpm test` after every step.
- [Circular imports] → `Instances` ↔ `InstanceDiagnostics`/ExecutableOrigin may share types; break cycles at type-only level.

## Validation

`pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`.
