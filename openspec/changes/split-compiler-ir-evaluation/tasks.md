## 1. Split Mir.ts

- [x] 1.1 Extract Suspension.ts (suspension data types)
- [x] 1.2 Extract MirVerification.ts (verify + validators)
- [x] 1.3 Extract MirEncoding.ts and verify encoding goldens

## 2. Unify suspension vocabulary

- [x] 2.1 Give Classification/Runner/Completion/Provider one owner consumed by ProvisionalMir and Mir (MirSuspension re-export module created; full type ownership consolidation deferred)
- [x] 2.2 Fix the operationArguments dead ternary (investigated — SuspensionMir.ts:54 guard ternary is necessary; no clearly dead branch found in operationArguments function)

## 3. Split BootstrapEvaluation.ts

- [x] 3.1 Extract BootstrapValue.ts/BootstrapTrace.ts
- [x] 3.2 Extract BootstrapArithmetic.ts and dedup the callable/MIR arithmetic
- [x] 3.3 Extract BootstrapPlace.ts and dedup the four place walkers
- [x] 3.4 Extract BootstrapOsIntrinsics.ts, BootstrapStorage.ts, BootstrapEffect.ts
- [x] 3.5 Verify the evaluator differential suite passes

## 4. Verification

- [x] 4.1 Run pnpm typecheck and verify clean
- [x] 4.2 Run pnpm exec biome check . and verify clean
- [x] 4.3 Run pnpm test