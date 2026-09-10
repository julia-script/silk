## 1. Source actors

- [x] 1.1 Implement and document both HMAC actors over SHA-2; verify known-answer and incremental acceptance cases.
- [x] 1.2 Implement and document both HKDF actors and OutputTooLongError; verify RFC and pinned independent vectors, empty inputs, and output bounds.

## 2. Integration and evidence

- [x] 2.1 Register manifest modules, regenerate surfaces, and update the prescriptive reference; verify stdlib and documentation checks.
- [x] 2.2 Add a consolidated shared native corpus program with distinct known-answer and boundary cases; verify focused execution and measure its incremental cost against the base.

## 3. Verification and handoff

- [x] 3.1 Run typecheck, format:check, lint, test, check, and release:candidate in the required order and record results.
- [ ] 3.2 Complete independent code review and mandatory test-economics review; resolve findings and obtain approval of the exact committed diff.
      Draft PR publication and Linear handoff follow the silk-work delivery workflow after these implementation tasks pass.

Verification passed: `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`,
`pnpm check`, and `pnpm release:candidate`. The final test/check runs used
`TURBO_ENV_MODE=loose VITEST_MAX_WORKERS=4` with idle sleep inhibited. Compiler evidence includes
2,445 semantic tests, 328 native acceptance tests, 91 documented modules, and 56 doctests; the LSP
suite passed all 149 tests, and release-candidate validation passed all 10 tests. Earlier host-load
timeouts and two tests interrupted by a confirmed macOS sleep interval passed in the final run.
