## Context

All core phases exist but must be exposed through one stable compilation operation and consumer CLI. The tutorial promise is not met until the confirmed source compiles and executes independently of the playground. See `proposal.md` for motivation.

## Goals / Non-Goals

**Goals:**

- Compose the complete Effect pipeline.
- Produce clean textual IR suitable for shell redirection and Clang.
- Validate the guided completion result end to end.

**Non-Goals:**

- Run Clang inside the LLVM package or browser.
- Design a production CLI framework.
- Add optimization, printing, or a standard library.

## Decisions

### Expose one `Compiler.compile(source)` Effect returning structured phase artifacts plus final IR

The CLI can select IR while tests/playground inspect tokens and AST. Preserving stage outputs avoids reparsing and supports diagnostics.

### Reserve stdout exclusively for valid IR and stderr for diagnostics

This makes `tiny source.tiny > score.ll` reliable without an output-file subsystem in the learning core.

### Keep native compilation as a shell checkpoint and CI validation step

The compiler remains portable and serverless; Clang is an explicit external consumer.

### Use `score.tiny` as the single acceptance fixture

One central program covers the confirmed language features and prevents unrelated sample complexity.

## Risks / Trade-offs

- [Risk] Structured compiler output complicates the beginner API → Present the CLI first and reveal the stage object only when explaining playground reuse.
- [Risk] Native exit codes are limited → Keep expected values below 256 and state that this is an observation mechanism, not Tiny I/O.
- [Risk] Packed-package E2E tests are flaky → Pin tools and isolate build/output directories.

## Migration Plan

Complete CLI integration, add `score.tiny`, end-to-end tests, and Lesson 11. Enable the documentation validation job only after it passes from a clean checkout.
