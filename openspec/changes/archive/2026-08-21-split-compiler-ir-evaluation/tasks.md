## 1. Split Mir.ts

- [ ] 1.1 Extract Suspension.ts (suspension data types)
- [ ] 1.2 Extract MirVerification.ts (verify + validators)
- [ ] 1.3 Extract MirEncoding.ts and verify encoding goldens

## 2. Unify suspension vocabulary

- [ ] 2.1 Give Classification/Runner/Completion/Provider one owner consumed by ProvisionalMir and Mir
- [ ] 2.2 Fix the operationArguments dead ternary

## 3. Split BootstrapEvaluation.ts

- [ ] 3.1 Extract BootstrapValue.ts/BootstrapTrace.ts
- [ ] 3.2 Extract BootstrapArithmetic.ts and dedup the callable/MIR arithmetic
- [ ] 3.3 Extract BootstrapPlace.ts and dedup the four place walkers
- [ ] 3.4 Extract BootstrapOsIntrinsics.ts, BootstrapStorage.ts, BootstrapEffect.ts
- [ ] 3.5 Verify the evaluator differential suite passes

## 4. Verification

- [ ] 4.1 Run pnpm typecheck and verify clean
- [ ] 4.2 Run pnpm exec biome check . and verify clean
- [ ] 4.3 Run pnpm test