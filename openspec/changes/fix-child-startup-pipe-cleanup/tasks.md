## 1. Partial startup cleanup

- [x] 1.1 Add a controlled generated-boundary fixture proving exactly-once cleanup for configuration and acquisition failures, primary errno preservation, no fork on failure, and successful startup handoff; demonstrate the regression fails before the fix.
- [x] 1.2 Split notice acquisition/configuration and verify the native boundary fixture passes.

## 2. Integration

- [ ] 2.1 Run typecheck, format:check, lint, test, check, and release:candidate in the required order; record exact outcomes.
