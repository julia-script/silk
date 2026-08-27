## 1. Consumer scaffold

- [x] 1.1 Create a standalone `examples/tiny-language` TypeScript project that is not dependent on workspace-only imports.
- [x] 1.2 Add release-ready npm dependency declarations plus a documented local packed-package substitution for pre-release validation.
- [x] 1.3 Add the supplied application-edge CLI/runtime scaffold without teaching filesystem or process handling as compiler logic.

## 2. Setup lesson

- [x] 2.1 Create Lesson 3 with Node, pnpm, TypeScript, Effect, and Clang prerequisite checks.
- [x] 2.2 Add the empty-module smoke program using public `@silk-lang/llvm/Builder` and `@silk-lang/llvm/IrText` imports.
- [x] 2.3 Document recovery for package resolution, private import, TypeScript execution, and missing-Clang problems.

## 3. Validation

- [x] 3.1 Build and pack `@silk-lang/llvm`, install it into the clean consumer scaffold, and run its typecheck/smoke command.
- [x] 3.2 Link Lesson 3 into the numbered series and verify setup commands from a clean directory.
- [x] 3.3 Run `pnpm check` and `pnpm release:candidate`, reporting any pre-existing failure separately.
