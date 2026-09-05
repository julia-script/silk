## 1. Correct header elaboration

- [x] 1.1 Replay inherent owner lifetime elision and complete member binder inheritance; verify declaration acceptance, elided invocation and explicit comparison with frontend-only tests.
- [x] 1.2 Preserve fixed Self semantics and whole-family validation; verify resolved Self, invalid independent input, and existing rejected-head controls.

## 2. Reconcile and verify

- [x] 2.1 Clarify reference and canonical specification, remove diagnostic probes, and pass strict OpenSpec validation.
- [x] 2.2 Run focused semantic checks, verify the original CLI reproducer, and complete applicable repository checks; record exact evidence and deferred reviewer passes.

## Verification evidence

- The original elided constructor invocation failed with SEM0010 before the fix; declaration-only and explicit-lifetime controls passed.
- Focused semantic verification passed: 87 tests across InherentImpl, MethodCall and DeclarationIndex. The final InherentImpl file has 11 passing tests, including local builtin-shadowing owner identity.
- The rebuilt CLI accepts the exact reproducer with `check --target aarch64-apple-darwin`.
- `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, strict OpenSpec validation and `pnpm check` passed. The full check completed 33 tasks in 25m15s, including 2,248 compiler tests, 310 native acceptance cases and 17 script checks.
- `pnpm release:candidate` passed all 10 packaging checks in 31.07s.
- One paired focused measurement against db05ef34: base 249.7 ms for 4 tests; branch 281.9 ms for 11 tests (+32.2 ms observed). The seven new frontend cases totaled 73.4 ms. Both measurements ran while broader validation was active; no timing assertion or additional backend execution was added.
- General and test-economics reviewer passes remain deferred at Julia's request. Measurements above were collected by the coordinator.
