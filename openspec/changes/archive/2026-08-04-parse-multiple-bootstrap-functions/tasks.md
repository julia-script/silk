## 1. Multi-Function Parser

- [x] 1.1 Add readable two-function, three-function, missing-first-brace, inter-function punctuation,
      trailing-trivia, and empty-source fixtures with exact expected token coverage.
- [x] 1.2 Extend source-file parsing to produce a non-empty ordered sequence of direct function
      declarations before EOF without changing the existing function-local grammar.
- [x] 1.3 Synchronize a missing right brace on a following `PubKeyword` and retain unexpected input
      between declarations without swallowing the next function.
- [x] 1.4 Preserve deterministic parser diagnostic ordering, progress, frozen data, and exact
      lossless token identity across every multi-function fixture.
- [x] 1.5 Test one, two, and three declarations, inter-function trivia ownership, empty recovery,
      damaged-first-function recovery, unexpected boundaries, and repeated determinism.

## 2. First-Only Semantic Boundary

- [x] 2.1 Keep semantic analysis total for multi-function parse results while explicitly analyzing
      only the first direct declaration in this change.
- [x] 2.2 Add focused tests proving later syntax does not alter the first function's facts or
      diagnostics and document the temporary first-only boundary.

## 3. Package and Visual Feedback

- [x] 3.1 Update compiler README grammar examples and add a Changesets entry for multi-function
      concrete syntax without claiming declaration collection exists.
- [x] 3.2 Extend release-candidate validation for the changed packed parser behavior and confirm no
      inspector or source files enter the published package.
- [x] 3.3 Add two-function and damaged-first-function inspector presets and render separate top-level
      concrete branches in source order.
- [x] 3.4 Add an accessible first-function-only semantic notice and browser-test that the concrete
      second declaration stays visible through recovery.

## 4. Verification

- [x] 4.1 Run strict OpenSpec validation and focused compiler/docs typecheck, test, format,
      production-build, and browser-smoke commands.
- [x] 4.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and
      `pnpm release:candidate`, fixing every introduced failure.
