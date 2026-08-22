## 1. Remove zero-caller facade exports

- [x] 1.1 Delete the nine zero-caller `Analysis` exports and verify typecheck finds no callers
- [x] 1.2 Move the test-only `*Of` projections to `test/support/` and update their imports

## 2. Delete dead serializers, guards, and narrowers

- [x] 2.1 Delete `IntrinsicAvailability.encode`, `Type.isOutOfMemoryError`, `intrinsicConformances`/`intrinsicallyConforms`, `ConformanceGoal.dependencies`, `OsRuntime.isSymbol`
- [x] 2.2 Delete `CallableFieldRealization.callableRealizationOf`/`effectRealizationOf`, `OpaqueRealization.publicOrigin`
- [x] 2.3 Delete `SuspensionMir.hasSuspension`, `Hir.hasUnavailable`, `Mir.suspensionControlEdges`

## 3. Tooling and index cleanup

- [x] 3.1 Delete `ModuleTooling.make` and the `DeclarationIndex.presentParameterNameEntries` alias
- [x] 3.2 Drop the `FrontendTooling` re-exports of `ModuleTooling` symbols and import `ModuleTooling` directly in `Analysis`

## 4. Dead hot-path work and prod fixtures

- [x] 4.1 Remove the eager `SyntaxCorrespondence.between` call and `Changed.correspondence`
- [x] 4.2 Relocate `Mir.samples` to `test/support/mirSamples.ts` and drop the `effect/Option` import from `Mir`

## 5. Verification

- [x] 5.1 Run `pnpm typecheck` and verify clean
- [x] 5.2 Run `pnpm exec biome check .` and verify clean
- [x] 5.3 Run `pnpm test` and verify the suite passes
