## Context

See proposal.md. The parser already threads an immutable `State` and a `syntaxNode`/recovery core, which is the seam the split preserves.

## Decisions

- **`internal/ParseState.ts`**: move `State`, `expect`, `syntaxNode`, trivia-skipping, and `peek(n)` here. Each grammar sub-module takes `State` and returns nodes.
- **`parseServiceLikeDeclaration(kind: 'service' | 'interface')`**: the two copies differ only in the keyword token and a loop-guard terminator; parameterize those.
- **`parseCallableContract()`**: produces the return/failure/requirement/where elements consumed by both operation and function declarations.
- **`peek(n)` lookahead**: one trivia-skipping scan primitive; the two statement lookahead predicates become token-kind predicates, removing the re-parse cost and double-encoded grammar decision.
- **`keywordSpellings`**: move `fn`/`let`/`move`/`pub`/`return`/`import` into the table and delete the byte-compare block (`Lexer.ts:136-176`).

## Risks / Trade-offs

- [Span/diagnostic drift] → parser golden/span tests are the net; keep `State` threading identical so offsets do not change.
- [Cycles] → grammar sub-modules may import each other; keep at type-only level (`import type`) or hoist shared node constructors into `ParseState`.

## Validation

`pnpm typecheck`, `pnpm exec biome check .`, `pnpm test` (parser golden + span suites).
