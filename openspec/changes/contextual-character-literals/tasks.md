## 1. Literal selection

- [x] 1.1 Normalize context-selected character expressions and primitive constants into integer values; verify immediate contexts and both operand directions in CharacterLiteral tests.
- [x] 1.2 Extend static syntax literal evaluation; verify char default, integer selection and rejection with the existing StaticText primitive test.
- [x] 1.3 Generalize SEM0002 to selected type and exact bounds; verify structured diagnostic codes, spans and reasons, and regenerate the diagnostic catalog.

## 2. Contract and integration

- [x] 2.1 Update lexical and value reference text and examples; verify OpenSpec strict validation and generated documentation checks.
- [x] 2.2 Verify selected integer MIR structure and a shared native acceptance witness, preserving existing char behavior.
- [ ] 2.3 Run typecheck, format:check, lint, test, check and release:candidate; resolve failures and obtain independent code and test-economics approval of the final committed diff.

Verification handoff: typecheck, format:check, lint, focused compiler tests, the native character witness, documentation checks and strict OpenSpec validation passed locally. Broad test runs encountered timeouts; isolated reruns of the two compiler files passed. On 2026-09-07 Julia directed the remaining broad verification to PR CI because other local sessions are running tests. The full check and release-candidate gates remain pending CI.
