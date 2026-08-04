# Roadmap — Silk Effect

> Direction, not commitment — Now is committed; Next is planned; Later is exploration.
> Only Now items may be promised to anyone. This document changes as we learn.
> Last reviewed: 2026-08-04 · Review cadence: after each OpenSpec archive, or monthly when no
> change ships · Scope: whole project

## Vision

Silk Effect will be a low-level systems language that combines explicit memory and execution
control with typed failures, explicit service requirements, deterministic resource scopes, and
tooling-friendly semantics. The first destination is the smallest coherent language capable of
compiling its own compiler; broader language and ecosystem work follows evidence from that
self-hosting core.

**Current objective:** prove a checkable two-function Silk program — measured by `main` calling a
uniquely resolved `answer`, receiving its `I32` type, and remaining deterministic and inspectable
across valid, missing, duplicate, ambiguous, and syntax-damaged sources.

## Column rules

- **Now** — problem validated, solution shaped, actively worked or next up. Committed.
- **Next** — problem chosen and understood; solution still in discovery. Planned, not promised.
- **Later** — problem worth solving, no solution chosen. Options, not a queue.

## Now

### Prove the first cross-declaration semantic relationship

- **Problem:** One checked function proves local facts, but it cannot validate declaration
  collection, ambiguity, expression references, or source-order-independent lookup.
- **Outcome & done-when:** A two-function source can parse both declarations, collect stable facts,
  preserve a zero-argument call, resolve `main → answer`, propagate `I32`, and show every stage and
  failure state in the hidden inspector without introducing AST, HIR, or execution.
- **Status:** shaped — four dependency-ordered OpenSpec changes are ready; implement, visually
  inspect, sync, and archive exactly one before reassessing the remaining changes.
- **Appetite:** four focused changes with a hard review boundary after each sync; parameters,
  arguments, arithmetic, local bindings, general scopes, AST, HIR, MIR, and lowering remain outside
  this milestone.
- **Links:** changes: `parse-multiple-bootstrap-functions` →
  `collect-bootstrap-declarations` → `parse-first-function-call` →
  `resolve-first-function-call` ·
  [bootstrap syntax spec](../openspec/specs/bootstrap-syntax/spec.md) ·
  [bootstrap semantic facts](../openspec/specs/bootstrap-semantic-facts/spec.md) ·
  [bootstrap-language map](../wayfinder/bootstrap-language/map.md) ·
  [compiler pipeline decision](../wayfinder/bootstrap-language/issues/06-bootstrap-compiler-pipeline.md)

## Next

### Find the next pressure that earns a semantic representation

- **Problem:** Even a resolved two-function program has only explicit return types, integer literals,
  and zero-argument calls, so it does not prove which next language feature should shape AST/HIR.
- **Hypothesis:** Comparing the real implementation pressure from signed integers, one parameter and
  argument, arithmetic, or a local binding will reveal the smallest next coherent milestone.
- **Confidence:** low until the resolved-call milestone is complete.
- **Assumes:** Direct facts over the CST remain sufficient through the first reference — unvalidated.
- **Open questions:** Which candidate creates necessary semantic structure rather than completeness
  polish, and does that structure finally justify a semantic AST or HIR?

## Later

- **Lower checked programs into backend-neutral execution** — introduce HIR and MIR only when
  concrete declarations, types, control flow, ownership, failures, and services require those
  representations.
- **Produce the first native Silk program** — connect proven MIR to `@silk-effect/llvm`, scoped
  artifacts, Clang, native linking, and transactional output.
- **Supply the compiler's native platform** — add the minimum runtime, standard library, host
  services, and private C shim in response to real self-hosting compiler needs.
- **Make Silk capable of expressing its own compiler** — progressively replace the TypeScript seed
  implementation with Silk modules while preserving reference equivalence; revisit the exact port
  boundaries once the frontend, MIR, and runtime interfaces have survived real use.
- **Prove native self-hosting** — produce stage 1, stage 2, and a byte-identical fixed-point rebuild
  with complete conformance, failure, debug, resource, and performance evidence on all required
  native hosts.
- **Grow beyond the bootstrap subset** — concurrency, networking, schemas, observability, richer
  tooling, and broader standard-library families become candidates only after the self-hosting core
  exposes their real constraints.
- **Deepen WebAssembly integration** — preserve backend-neutral boundaries now; consider a direct
  WebAssembly backend and generated Effect interop after the native bootstrap path is accepted.

## Maintenance budget

Reserve approximately 20% of project capacity for keeping the foundation trustworthy.

- Keep `@silk-effect/llvm` aligned with its pinned upstream baseline, deterministic fixtures, Effect
  architecture rules, and packed release-candidate checks.
- Keep OpenSpec capabilities, archived changes, the Wayfinder decision index, and this roadmap
  synchronized with implementation discoveries instead of allowing multiple competing truths.

## Not doing

- General concurrency, atomics, async scheduling, networking, or a broad user-facing FFI during
  bootstrap — the compiler workload does not require them.
- A direct WebAssembly backend as a self-hosting prerequisite — backend-neutral MIR is sufficient
  protection until native self-hosting succeeds.
- A package registry, dependency solver, production build system, or full language server — none is
  required to prove the bootstrap language.
- A general incremental query engine — immutable analysis snapshots and localized deterministic
  worklists are the accepted bootstrap architecture.
- Backward compatibility for unreleased compiler APIs — early implementation evidence should be
  allowed to improve boundaries rather than fossilize them.

## Open questions

- What executable name and public analysis facade should eventually accompany
  `@silk-effect/compiler`?
- Should Silk compiler modules replace their TypeScript counterparts continuously as capabilities
  land, or should the first port begin after the stage-0 subset is feature-complete?
- After the resolved-call milestone, which pressure should come first: signed literals, one
  parameter and argument, arithmetic, or a local binding?

## Changelog

- 2026-08-04: Shipped and archived `analyze-first-bootstrap-function` in commit `373c4d8`; direct
  declaration, `I32`, integer, compatibility, and semantic diagnostic facts held without AST/HIR.
  Recast Now as a checkable two-function milestone split into four dependency-ordered changes, each
  with a required inspector checkpoint and a sync/reassessment boundary.
- 2026-08-04: Shipped and archived `parse-first-bootstrap-function` in commit `ba6feaf`; its
  lossless tree, bounded recovery, deterministic diagnostics, and hidden inspector met the recorded
  outcome. Promoted one-function declaration and `I32` fact analysis to Now, explicitly keeping HIR
  behind evidence from a second semantic form.
- 2026-08-04: Shipped exact source text and lossless lexing, then promoted a one-function concrete
  syntax tree and direct-link inspector to Now; semantic interpretation remains Next.
- 2026-08-04: Replaced the oversized end-to-end compiler-kernel initiative with source text and
  lexing. Moved parsing, semantic facts, HIR/MIR, native code generation, and runtime work behind
  evidence-producing capability boundaries.
- 2026-08-04: Created after completing the bootstrap-language Wayfinder map and archiving the LLVM
  builder and Tiny-language OpenSpec portfolios. The first bet is an end-to-end compiler kernel,
  followed by frontend semantics, ownership-aware lowering, and the native bootstrap platform.
