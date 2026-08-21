## 1. Split Lower.ts

- [ ] 1.1 Move `FunctionLowering` into its own state module
- [ ] 1.2 Extract `Forwarding.ts` and `ValueType.ts`
- [ ] 1.3 Extract `EffectLowering.ts` and `WitnessLowering.ts`
- [ ] 1.4 Extract `LowerExpression.ts` + `LowerBuiltin.ts`
- [ ] 1.5 Extract `CleanupEmission.ts`, `LowerStatements.ts`, `EntryAssembly.ts`
- [ ] 1.6 Verify lowering tests pass

## 2. Dedup loan choreography

- [ ] 2.1 Add `lowerProvidedEffect` and replace the four provider-loan copies
- [ ] 2.2 Route the five inline end-loan loops through `endLoans`/`endRunLoans`
- [ ] 2.3 Verify borrow-identity/loan-end determinism tests pass

## 3. Ownership factoring

- [ ] 3.1 Extract `CleanupPlan.ts` and `OwnershipEncoding.ts`
- [ ] 3.2 Share `inReleaseOrder` and route `SuspensionOwnership` through `Ownership.cleanupPlan`

## 4. Verification

- [ ] 4.1 Run `pnpm typecheck` and verify clean
- [ ] 4.2 Run `pnpm exec biome check .` and verify clean
- [ ] 4.3 Run `pnpm test`
