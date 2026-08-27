## Context

The complete AST can represent `fn main() = 42`, but there is no lowering path. This lesson must connect the prior LLVM vocabulary to actual public builder operations and then cross the application boundary into Clang. See `proposal.md` for motivation.

## Goals / Non-Goals

**Goals:**

- Lower the smallest valid Tiny program to readable IR.
- Demonstrate correct builder/body ownership and termination.
- Reach an early native executable with exit code `42`.

**Non-Goals:**

- Lower general arithmetic, calls, or conditionals.
- Emit bitcode or invoke Clang from `@silklang/llvm`.
- Hide LLVM IR behind a one-call abstraction before learners see its structure.

## Decisions

### Implement the first `Compiler.compile` path directly around one `i32` literal body

The concrete path mirrors the annotated IR and becomes the base for recursive lowering.

### Use `i32 ()` for `main` and `Constant.integerSigned` for literals

This matches the fixed Tiny type model and the host executable contract.

### Render textual IR and invoke Clang in a documented external command

Readable `.ll` is the teaching artifact; process execution remains outside the package and compiler core.

### Keep body creation inside `Function.buildBody`

The transaction exposes missing terminators and ownership mistakes at the correct boundary.

## Risks / Trade-offs

- [Risk] Platform exit-code commands differ → Lead with POSIX and include one compact PowerShell equivalent.
- [Risk] Clang diagnostics distract from frontend work → Validate the exact generated fixture in CI and provide the known-good IR excerpt.
- [Risk] Learners copy builder calls without connecting them to IR → Pair every operation with the line it produces.

## Migration Plan

Add the initial `Compiler.ts`, minimal fixture/test, native validation command, and Lesson 7. Later changes generalize this compiler path; rollback leaves earlier frontend lessons intact.

