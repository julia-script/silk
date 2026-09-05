## 1. Metadata classification

- [x] 1.1 Add a generated-boundary fixture proving WrongType under stale errno and preservation of failed-fstat errno, with exactly one descriptor close; demonstrate failure before the fix.
- [x] 1.2 Split the metadata failure/kind branches and verify the focused filesystem tests pass.

## 2. Integration

- [ ] 2.1 Run typecheck, format:check, lint, test, check, and release:candidate in the required order; record exact outcomes.
