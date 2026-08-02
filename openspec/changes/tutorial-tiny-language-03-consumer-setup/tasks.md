## 1. Consumer scaffold

- [ ] 1.1 Create a standalone `examples/tiny-language` TypeScript project that is not dependent on workspace-only imports.
- [ ] 1.2 Add release-ready npm dependency declarations plus a documented local packed-package substitution for pre-release validation.
- [ ] 1.3 Add the supplied application-edge CLI/runtime scaffold without teaching filesystem or process handling as compiler logic.

## 2. Setup lesson

- [ ] 2.1 Create Lesson 3 with Node, pnpm, TypeScript, Effect, and Clang prerequisite checks.
- [ ] 2.2 Add the empty-module smoke program using public `@silk-effect/llvm/Builder` and `@silk-effect/llvm/IrText` imports.
- [ ] 2.3 Document recovery for package resolution, private import, TypeScript execution, and missing-Clang problems.

## 3. Validation

- [ ] 3.1 Build and pack `@silk-effect/llvm`, install it into the clean consumer scaffold, and run its typecheck/smoke command.
- [ ] 3.2 Link Lesson 3 into the numbered series and verify setup commands from a clean directory.
- [ ] 3.3 Run `pnpm check` and `pnpm release:candidate`, reporting any pre-existing failure separately.


