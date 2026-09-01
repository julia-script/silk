## Context

The package is not published yet, but the tutorial must read like consumer documentation and must never rely on repository-private imports. The repository currently has no top-level examples workspace. See `proposal.md` for motivation.

## Goals / Non-Goals

**Goals:**

- Create a reproducible consumer-shaped project that later lessons can extend.
- Validate the public package surface from a packed local artifact.
- Produce an early successful LLVM rendering checkpoint.

**Non-Goals:**

- Teach package-management internals.
- Teach filesystem or process services as compiler concepts.
- Compile a native program yet.

## Decisions

### Keep the example outside the monorepo workspace contract

Use `examples/tiny-language` as a standalone consumer project and test it against a packed package. A `workspace:*` dependency would hide packaging/export mistakes.

### Show npm-style commands in prose and isolate the local-pack substitution in an author note

The learner experience stays release-ready while maintainers retain a deterministic pre-release path.

### Supply the application-edge CLI shell

Filesystem input, stdout, stderr, and runtime execution are infrastructure rather than learning objectives; later lessons fill compiler actors behind the boundary.

### Use only explicit public package subpaths

This mirrors intended consumption and keeps the package barrel from growing.

## Risks / Trade-offs

- [Risk] Packed-package validation becomes slow → Build and pack once per documentation validation job, then reuse the artifact.
- [Risk] Node's TypeScript execution flags change → Pin the tested Node version and keep the example typecheckable with ordinary `tsc`.
- [Risk] Clang absence blocks a lesson that only renders IR → Treat Clang verification as a prerequisite warning until Lesson 7.

## Migration Plan

Add the standalone example scaffold and Lesson 3 page. CI integration can first run as an opt-in documentation check, then become required once stable. Rollback removes only the example and page.
