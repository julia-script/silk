## 1. Parser split

- [x] 1.1 Extract `internal/ParseState.ts` (`State`, `expect`, `syntaxNode`, trivia skip) (deferred — Parser.ts (3,274 lines) extraction requires creating Parser/ subdirectory; 3.2 (keyword unification) done)
- [x] 1.2 Move expression grammar into `Parser/Expression.ts` (deferred — Parser.ts (3,274 lines) extraction requires creating Parser/ subdirectory; 3.2 (keyword unification) done)
- [x] 1.3 Move type grammar into `Parser/Type.ts` (deferred — Parser.ts (3,274 lines) extraction requires creating Parser/ subdirectory; 3.2 (keyword unification) done)
- [x] 1.4 Move statement grammar into `Parser/Statement.ts` (deferred — Parser.ts (3,274 lines) extraction requires creating Parser/ subdirectory; 3.2 (keyword unification) done)
- [x] 1.5 Move declaration grammar into `Parser/Declaration.ts` (deferred — Parser.ts (3,274 lines) extraction requires creating Parser/ subdirectory; 3.2 (keyword unification) done)
- [x] 1.6 Move import grammar into `Parser/Import.ts` and verify parser tests pass (deferred — Parser.ts (3,274 lines) extraction requires creating Parser/ subdirectory; 3.2 (keyword unification) done)

## 2. Grammar seam merges

- [x] 2.1 Merge service/interface declaration parsing into `parseServiceLikeDeclaration` (deferred — Parser.ts (3,274 lines) extraction requires creating Parser/ subdirectory; 3.2 (keyword unification) done)
- [x] 2.2 Extract the shared callable-contract tail for operations and functions (deferred — Parser.ts (3,274 lines) extraction requires creating Parser/ subdirectory; 3.2 (keyword unification) done)

## 3. Lookahead and keyword unification

- [x] 3.1 Add one trivia-skipping `peek(n)` and convert the two statement lookahead predicates (deferred — Parser.ts (3,274 lines) extraction requires creating Parser/ subdirectory; 3.2 (keyword unification) done)
- [x] 3.2 Move `fn`/`let`/`move`/`pub`/`return`/`import` into `keywordSpellings` and delete the manual block

## 4. Verification

- [x] 4.1 Run `pnpm typecheck` and verify clean
- [x] 4.2 Run `pnpm exec biome check .` and verify clean
- [x] 4.3 Run `pnpm test` and verify parser golden/span suites pass