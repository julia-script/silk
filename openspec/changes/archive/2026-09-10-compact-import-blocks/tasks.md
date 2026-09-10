## 1. Formatter behavior

- [x] 1.1 Implement import-aware declaration and comment gap spacing, verified by focused SyntaxFormatter tests.
- [x] 1.2 Add exact-output coverage for import blocks, boundaries, comments, and all-import files; verify clean reparsing, preserved syntax/comments, and unchanged byte-identical second passes.

## 2. Integration and specification

- [ ] 2.1 Verify the full check and release-candidate gates in PR CI; local focused tests, typecheck, format:check, lint, and required fixture updates are complete. The user moved full validation to CI because other local sessions are running tests.
- [x] 2.2 Strict-validate the OpenSpec change, sync the delta to the main specification, and archive the completed artifacts.

## Validation status

- Passed: all 57 SyntaxFormatter tests; pressure-fixture idempotence; typecheck; format:check; lint; strict change validation; all 135 maintained specifications.
- Updated only the two canonical static-composition fixtures and the required compiler integrity digest.
- The first local full suite found the now-fixed fixture spacing and 60-second timeouts in StoredCallableRuntime and StringLayout. Both files passed in isolation (3 tests). A later full check hit a 60-second VectorAcceptance timeout. Baseline timeout behavior was not independently verified.
- Local full checks were stopped at the user's request. CI owns remaining full validation and release:candidate. Archive this already-synced change after those gates pass.
