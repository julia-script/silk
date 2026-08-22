## Context

See proposal.md. Each split follows a seam already present in the code. Public phase behavior is unchanged, while internal callers migrate to each final owning actor and no forwarding facade remains.

## Decisions

- **`Type.ts` → `TypeInference.ts`**: move `GenericArgumentConflict` through `substitution` (bind, infer, row/failure/requirement argument inference, `prefixSubstitution`) as one self-contained actor that only reads `Type` predicates. `Type.ts` keeps the vocabulary, keys/encoding, and row policies; inference callers import `TypeInference` directly.
- **`Instances.ts` split**: keep `discover`/`resolveEntry`/`specialize`/`keyOf`/`concreteConstraintEvidence` in `Instances`; move the implementations and supporting diagnostic vocabulary for `copyDropViolations`/`requirementBindingViolations`/`unlowerableWitnessViolations`/`storedExecutableViolations`/`violationDiagnostics` to `InstanceDiagnostics.ts`, with no forwarding pass back to `Instances`; move `callableOriginOf`/`effectOriginOf`/`resultEffectIdentity`/`concreteCallables`/`concreteEffects`/`suspensionGraph`/`suspendableNodes`/`reachableIntrinsics`/`interfaceWitnessTargets`/`callTargets` to `ExecutableOrigin.ts`.
- **`Pipeline.ts` split**: `realize` (Analysis-facing) and `prepare` (Driver-facing) share one `discoverAndLower(self, options)` returning `{ instances, diagnostics, target, layout, mir, report }`; each public entry maps that result through its own gates. `Frontend` owns phase orchestration, while `IncrementalReuse` owns `ProjectReuseBasis`, invalidation, structural sharing, and checkpointing.

## Risks / Trade-offs

- [Huge code motion] → do each actor extraction as its own commit; run `pnpm typecheck`/`pnpm test` after every step.
- [Circular imports] → `Instances` ↔ `InstanceDiagnostics`/ExecutableOrigin may share types; break cycles at type-only level.

## Validation

`pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`.
