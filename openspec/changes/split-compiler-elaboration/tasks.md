## 1. Extract the four actors

- [ ] 1.1 Extract `ExpressionAnalysis.ts` (and pull the three inline branches into named functions)
- [ ] 1.2 Extract `CallResolution.ts` (the 8566–9030 tail + its sub-actors)
- [ ] 1.3 Extract `StatementAnalysis.ts`
- [ ] 1.4 Extract `HirLowering.ts` and verify elaboration tests pass at each step

## 2. Merge the duplicated walks

- [ ] 2.1 Add the parameterized `lowerStatements` helper and delete `hirEffectStatements`
- [ ] 2.2 Route the `hirStatements` closure through it and verify determineism tests

## 3. Dedup borrow-id and access reduction

- [ ] 3.1 Add `argumentBorrowId`/`loanEndsOf` and replace the six-plus inline copies
- [ ] 3.2 Route all five access-reduction sites through `strongestEffectAccess`
- [x] 3.3 Replace the two `as SyntaxTree.Node` casts with a bound `arms.at(0)`

## 4. Verification

- [x] 4.1 Run `pnpm typecheck` and verify clean
- [x] 4.2 Run `pnpm exec biome check .` and verify clean
- [x] 4.3 Run `pnpm test`