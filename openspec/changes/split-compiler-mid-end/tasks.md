## 1. Split Lower.ts

- [x] 1.1 Move `FunctionLowering` into its own state module
- [x] 1.2 Extract `Forwarding.ts` and `ValueType.ts`
- [x] 1.3 Extract `EffectLowering.ts` and `WitnessLowering.ts`
- [x] 1.4 Extract `LowerExpression.ts` + `LowerBuiltin.ts`
- [x] 1.5 Extract `CleanupEmission.ts`, `LowerStatements.ts`, `EntryAssembly.ts`
- [x] 1.6 Verify lowering tests pass

## 2. Dedup loan choreography

- [x] 2.1 Add `lowerProvidedEffect` and replace the four provider-loan copies
- [x] 2.2 Route the five inline end-loan loops through `endLoans`/`endRunLoans`
- [x] 2.3 Verify borrow-identity/loan-end determinism tests pass

## 3. Ownership factoring

- [x] 3.1 Extract `CleanupPlan.ts` and `OwnershipEncoding.ts`
- [x] 3.2 Share `inReleaseOrder` and route `SuspensionOwnership` through `Ownership.cleanupPlan`

## 4. Verification

- [x] 4.1 Run `pnpm typecheck` and verify clean
- [x] 4.2 Run `pnpm exec biome check .` and verify clean
- [x] 4.3 Run `pnpm test`
