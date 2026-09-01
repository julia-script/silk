## 1. Dependency and Concrete Call Syntax

- [x] 1.1 Confirm `collect-bootstrap-declarations` is synced and archived and the canonical semantic
      spec contains ordered function facts and closed declaration lookup.
- [x] 1.2 Add the `CallExpression` concrete node kind and a bounded return-expression branch between
      the existing integer form and a zero-argument identifier call.
- [x] 1.3 Parse and retain callee/trivia/parentheses exactly without introducing an argument-list or
      generic postfix-expression framework.
- [x] 1.4 Recover missing callees and parentheses on structural tokens and retain unsupported call
      arguments in one lossless error region without consuming the block or following function.
- [x] 1.5 Add valid, trivia-heavy, missing-callee, missing-parenthesis, unsupported-argument, and
      damaged-call-before-next-function fixtures and parser tests.

## 2. Unresolved Call Facts

- [x] 2.1 Replace integer-only returned-expression fields with a closed immutable integer-or-call
      fact union while preserving existing integer behavior and provenance.
- [x] 2.2 Publish present call callees as explicitly unresolved with unavailable compatibility and
      publish damaged callees as unavailable without semantic diagnostic duplication.
- [x] 2.3 Test unresolved spelling/provenance, unavailable propagation, empty semantic diagnostics,
      integer/call isolation across functions, frozen data, and repeated determinism.

## 3. Public Compiler Boundary

- [x] 3.1 Update compiler README grammar and semantic examples to distinguish concrete call syntax
      from name resolution and to restate the AST/HIR boundary.
- [x] 3.2 Add a Changesets entry and extend packed root/deep release validation for the new concrete
      node and returned-expression union without undeclared dependencies.

## 4. Call Syntax Inspector

- [x] 4.1 Add valid-call, missing-callee, missing-parenthesis, and unsupported-argument presets.
- [x] 4.2 Render the concrete call subtree and unresolved/unavailable semantic states without showing
      an unknown-name error before resolution exists.
- [x] 4.3 Browser-test valid and damaged calls, integer/call mixtures, responsive layout, phase-owned
      diagnostics, and continued hidden-route/search behavior in the production build.

## 5. Verification

- [x] 5.1 Run strict OpenSpec validation and focused compiler/docs typecheck, test, format,
      production-build, and browser-smoke commands.
- [x] 5.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and
      `pnpm release:candidate`, fixing every introduced failure.
