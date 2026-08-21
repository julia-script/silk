## Context

See proposal.md. `Lower.lowerProgram` and `FunctionLowering` (the mutable transform state) are the stable seams; the split factors the helper families around them.

## Decisions

- **`Lower.ts` seams** (line ranges from the review): `Forwarding.ts` (325–818), `ValueType.ts` (820–1368), `EffectLowering.ts` (1369–2171), `WitnessLowering.ts` (2918–3371), `LowerExpression.ts` (3371–5691, with `LowerBuiltin.ts` 5017–5687), `CleanupEmission.ts` (5693–6338), `LowerStatements.ts` (6339–7520), `EntryAssembly.ts` (7520–8206). `FunctionLowering` moves to its own state module.
- **`lowerProvidedEffect(fn, recipe, success, span, requirements)`**: one helper owning the BeginLoan + borrow + loanLocals.delete + dropOwnedProvider choreography; the four near-identical 90-line blocks delegate to it.
- **End-loan loops**: route `finishBuiltin`, `Call`, `BoundOperationCall`, the Run recipe, and the Os path through `endLoans`/`endRunLoans`; make `finishBuiltin` dedupe via a `Set` of ended keys and drop the missing-loan `continue` vs `return undefined` disagreement.
- **`CleanupPlan.ts`**: `CleanupPlan` type + `cleanupPlan`/`specializeCleanup`/`cleanupFields`/`cleanupTypeAtPath`/`realizedCallableCleanup`/`cleanupHas*`.
- **`OwnershipEncoding.ts`**: `encode` + the five private formatters (spanText/identityLabel/verdictText/siteText/cleanupText).
- **`inReleaseOrder`**: shared helper; `SuspensionOwnership`/Lower`` import it; `SuspensionOwnership` keeps delegating to `Ownership.cleanupPlan`.

## Risks / Trade-offs

- [Correctness-critical borrow choreography] → after dedup, run the ownership/lowering and wasm/native suspension determinism tests and the fail-ordinal sweep.
- [Large code motion] → one actor per commit, typecheck+test after each.

## Validation

`pnpm typecheck`, `pnpm exec biome check .`, `pnpm test` (ownership/lowering/suspension suites incl. boundary points).
