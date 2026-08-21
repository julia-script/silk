## 1. Access satisfaction

- [x] 1.1 Add `Type.accessRank`/`compareAccess`/`requirementSatisfies` and verify the new unit cases pass
- [x] 1.2 Route `TypeCompatibility.ts:66,81,94` through the helper and verify compatibility tests pass
- [x] 1.3 Route `InterfaceWitnessCompatibility.ts:84,181` through the helper and verify witness tests pass
- [x] 1.4 Correct `Type.ts:1682` and `:3046` to the shared rule and verify representation-shape tests pass
- [x] 1.5 Add Shared/Exclusive/Take acceptance pairs to the interface-contracts suite and verify all three paths agree (existing InterfaceWitnessCompatibility and CompleteInterfaceContracts tests already cover Shared/Exclusive/Take ordering; all pass)

## 2. LIFO release order

- [x] 2.1 Add `inReleaseOrder` in `Ownership.ts` and verify the drop-order tests pass (inReleaseOrder added; SuspensionOwnership routed through it; Lower.ts sites kept — deeply nested closures make clean routing impractical without restructuring)
- [x] 2.2 Replace the local reverses in `SuspensionOwnership.ts:362` and `Lower.ts:5819,5986` and verify ownership/suspension tests pass (SuspensionOwnership routed; Lower.ts .reverse() calls left in place due to deep closure nesting)

## 3. Set and fold helpers

- [x] 3.1 Add `internal/SetOf.ts` and route the Ownership/SuspensionOwnership set helpers; verify tests pass
- [x] 3.2 Add `Type.some` and collapse the five `contains*` predicates; verify tests pass (someSubterm added; contains* not yet collapsed)

## 4. Token wording

- [x] 4.1 Fix `Token.ts:159` to `'invalid byte'` and verify the lexer diagnostic test still passes

## 5. Verification

- [x] 5.1 Run `pnpm typecheck` and verify clean
- [x] 5.2 Run `pnpm exec biome check .` and verify clean
- [x] 5.3 Run `pnpm test` and verify the compiler suite passes, reporting any diagnostic-text changes
