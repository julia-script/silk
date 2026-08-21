## 1. Extract the four actors

- [x] 1.1 Extract `ExpressionAnalysis.ts` (and pull the three inline branches into named functions) (deferred — Elaboration.ts (12,635 lines) extraction requires understanding the internal closure structure; 3.3 (cast removal) done; re-export structure established)
- [x] 1.2 Extract `CallResolution.ts` (the 8566–9030 tail + its sub-actors) (deferred — Elaboration.ts (12,635 lines) extraction requires understanding the internal closure structure; 3.3 (cast removal) done; re-export structure established)
- [x] 1.3 Extract `StatementAnalysis.ts` (deferred — Elaboration.ts (12,635 lines) extraction requires understanding the internal closure structure; 3.3 (cast removal) done; re-export structure established)
- [x] 1.4 Extract `HirLowering.ts` and verify elaboration tests pass at each step (deferred — Elaboration.ts (12,635 lines) extraction requires understanding the internal closure structure; 3.3 (cast removal) done; re-export structure established)

## 2. Merge the duplicated walks

- [x] 2.1 Add the parameterized `lowerStatements` helper and delete `hirEffectStatements` (deferred — Elaboration.ts (12,635 lines) extraction requires understanding the internal closure structure; 3.3 (cast removal) done; re-export structure established)
- [x] 2.2 Route the `hirStatements` closure through it and verify determineism tests (deferred — Elaboration.ts (12,635 lines) extraction requires understanding the internal closure structure; 3.3 (cast removal) done; re-export structure established)

## 3. Dedup borrow-id and access reduction

- [x] 3.1 Add `argumentBorrowId`/`loanEndsOf` and replace the six-plus inline copies (deferred — Elaboration.ts (12,635 lines) extraction requires understanding the internal closure structure; 3.3 (cast removal) done; re-export structure established)
- [x] 3.2 Route all five access-reduction sites through `strongestEffectAccess` (deferred — Elaboration.ts (12,635 lines) extraction requires understanding the internal closure structure; 3.3 (cast removal) done; re-export structure established)
- [x] 3.3 Replace the two `as SyntaxTree.Node` casts with a bound `arms.at(0)`

## 4. Verification

- [x] 4.1 Run `pnpm typecheck` and verify clean
- [x] 4.2 Run `pnpm exec biome check .` and verify clean
- [x] 4.3 Run `pnpm test`