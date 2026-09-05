## 1. Partial startup cleanup

- [x] 1.1 Add a controlled generated-boundary fixture proving exactly-once cleanup for configuration and acquisition failures, primary errno preservation, no fork on failure, and successful startup handoff; demonstrate the regression fails before the fix.
- [x] 1.2 Split notice acquisition/configuration and verify the native boundary fixture passes.

## 2. Integration

- [x] 2.1 Run typecheck, format:check, lint, test, check, and release:candidate in the required order; record exact outcomes.

## Validation

The generated-boundary regression failed before the fix with no close attempt for either notice endpoint after failed fcntl. All 34 native-toolchain tests pass after the fix, including six-endpoint cleanup, four-endpoint acquisition-failure cleanup, original errno preservation, and successful startup handoff.

On 2026-09-05, `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`, `pnpm check`, and `pnpm release:candidate` all passed. The combined check passed 2,271 compiler tests, 317 native acceptance tests, 60 standard-library doctests, the other workspace suites, and 17 script checks. Release-candidate validation passed all 10 tests.

Two earlier `pnpm test` attempts failed with environmental `ENOSPC` errors: the first while ModuleVerification1 and ModuleVerification3 wrote temporary bitcode, and the second while Vite wrote temporary modules. Reclaiming generated native final-cache entries and backend-cache entries older than 24 hours restored capacity. The complete suite then passed through `pnpm check`, followed by a successful standalone `pnpm test` using those cached results. No production-code workaround was needed for the storage failures.
