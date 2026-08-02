## 1. Pipeline integration

- [ ] 1.1 Compose lexing, parsing, validation, two-pass lowering, and rendering in one named `Compiler.compile` Effect.
- [ ] 1.2 Return structured completed-stage artifacts for tests/playground reuse while keeping final textual IR directly available.
- [ ] 1.3 Finish the CLI so valid IR is written only to stdout and diagnostics only to stderr.

## 2. Completion lesson

- [ ] 2.1 Create Lesson 11 around the confirmed `abs`/`score`/`main` program and exit code `20`.
- [ ] 2.2 Add commands to emit `score.ll`, inspect the three functions, compile with Clang, and capture the process status immediately.
- [ ] 2.3 Add recovery for polluted stdout, hidden shell status, and malformed entry-point output.

## 3. End-to-end validation

- [ ] 3.1 Add a clean packed-package consumer test that compiles `score.tiny` to IR, invokes Clang, and asserts exit code `20`.
- [ ] 3.2 Confirm the written path has no dependency on the optional playground.
- [ ] 3.3 Run `pnpm check` and `pnpm release:candidate`, reporting any pre-existing failure separately.


