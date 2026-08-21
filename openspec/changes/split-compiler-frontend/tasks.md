## 1. Parser split

- [ ] 1.1 Extract `internal/ParseState.ts` (`State`, `expect`, `syntaxNode`, trivia skip)
- [ ] 1.2 Move expression grammar into `Parser/Expression.ts`
- [ ] 1.3 Move type grammar into `Parser/Type.ts`
- [ ] 1.4 Move statement grammar into `Parser/Statement.ts`
- [ ] 1.5 Move declaration grammar into `Parser/Declaration.ts`
- [ ] 1.6 Move import grammar into `Parser/Import.ts` and verify parser tests pass

## 2. Grammar seam merges

- [ ] 2.1 Merge service/interface declaration parsing into `parseServiceLikeDeclaration`
- [ ] 2.2 Extract the shared callable-contract tail for operations and functions

## 3. Lookahead and keyword unification

- [ ] 3.1 Add one trivia-skipping `peek(n)` and convert the two statement lookahead predicates
- [ ] 3.2 Move `fn`/`let`/`move`/`pub`/`return`/`import` into `keywordSpellings` and delete the manual block

## 4. Verification

- [ ] 4.1 Run `pnpm typecheck` and verify clean
- [ ] 4.2 Run `pnpm exec biome check .` and verify clean
- [ ] 4.3 Run `pnpm test` and verify parser golden/span suites pass
