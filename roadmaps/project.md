# Roadmap — Silk Effect

> Direction, not commitment — Now is committed; Next is planned; Later is exploration.
> Only Now items may be promised to anyone. This document changes as we learn.
> Last reviewed: 2026-08-05 · Review cadence: after each OpenSpec archive, or monthly when no
> change ships · Scope: whole project

## Vision

Silk Effect will be a low-level systems language that combines explicit memory and execution
control with typed failures, explicit service requirements, deterministic resource scopes, and
tooling-friendly semantics. The first destination is the smallest coherent language capable of
compiling its own compiler; broader language and ecosystem work follows evidence from that
self-hosting core.

**Current objective:** widen the bootstrap language over the accepted compiler spine — the full
source-to-native pipeline shipped 2026-08-05 with every phase inspectable, so growth now means
grammar and semantics (bindings, arithmetic, control flow, and onward toward issues 01–04 and 08)
riding an unchanged, determinism-gated architecture.

## Column rules

- **Now** — problem validated, solution shaped, actively worked or next up. Committed.
- **Next** — problem chosen and understood; solution still in discovery. Planned, not promised.
- **Later** — problem worth solving, no solution chosen. Options, not a queue.

## Now

### Widen the language, slice 1: bindings, arithmetic, branching

- **Problem:** The realigned spine (diagnostics through native link, all 13 changes archived
  2026-08-05) runs end to end over a grammar too small to exercise it: ownership is trivially
  satisfiable, lowering never emits `Branch` or `Drop`, and the differential harness compares only
  straight-line integer programs.
- **Outcome & done-when:** Silk programs with `let` bindings and `move`, signed literals with
  arithmetic, and `Bool`/comparison/`if` compile through every phase — real liveness ranges and
  cleanup drops in the ownership and MIR labs, real CFG diamonds, interpreter and native agreeing
  across both branch arms — with every artifact encoder and golden extended, not replaced.
- **Status:** shaped — dependency order pinned by the map: bindings first (the pressure issue 01
  deferred to issue 08's syntax), then arithmetic, then branching. Includes threading the `Backend`
  service through `Driver.CompileRequest` and `Analysis.codegen` instead of the current hardwired
  `LlvmBackend` call, so the declared seam is actually exercised.
- **Appetite:** three or four focused changes, one per feature, same
  propose → implement → inspect → archive loop as the realignment; structs, unions, match,
  failures, generics, and cross-module calls stay outside this slice.
- **Links:** [bootstrap-language map](../wayfinder/bootstrap-language/map.md) ·
  [ownership decision](../wayfinder/bootstrap-language/issues/01-ownership-lifetimes-and-scoped-allocation.md) ·
  [syntax decision](../wayfinder/bootstrap-language/issues/08-prototype-bootstrap-syntax.md) ·
  [realignment record](compiler-realignment.md)

## Next

### Give the language its data: structs, unions, and matching

- **Problem:** A compiler-shaped program is mostly data manipulation; expressions and branches
  alone cannot express tokens, trees, or facts.
- **Hypothesis:** Nominal structs first, then normalized structural unions with mode-aware
  exhaustive matching (issue 02), will force the first real type-layout decisions in MIR and the
  backend and make instance discovery meaningful.
- **Confidence:** medium — the semantic decisions are pinned; the MIR/layout consequences are not
  yet designed.
- **Assumes:** The slice-1 widening loop proves the extend-don't-replace economics of growing
  grammar, HIR, MIR, interpreter, backend, and labs together.
- **Open questions:** Where aggregate layout lives (MIR `TargetLayout` vs backend), and when typed
  failure rows (issue 03) and cross-module resolution (issue 04) interleave with the data work.

## Later

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
- How far should backend agnosticism go before a second backend exists? The `Backend` service and
  neutral MIR are pinned; the artifact hand-off (`bitcode` + Clang object step) deliberately stays
  LLVM-shaped per issue 06 until a real second backend pairs it with its own toolchain plan.

## Changelog

- 2026-08-05: Shipped and archived `bind-local-values` — slice 1's first change. `let` bindings
  and `move` now run through the whole spine: statement sequences in grammar and HIR, initializer
  inference, non-shadowing (`SEM0008`), the first real ownership analysis (liveness ranges, moves,
  `OWN0001` use-after-move with a new ownership diagnostic phase and `Violation` verdict),
  populated cleanup plans, lowered `Drop` operations, and interpreter/native trap parity across
  four new corpus programs. Backend injection landed alongside in `fa83a5f`. Next: arithmetic.
- 2026-08-05: Completed the compiler realignment — all 13 changes from
  [compiler-realignment](compiler-realignment.md) implemented and archived in one loop. The spine
  now runs source → module closure → declaration index → HIR → ownership → instances → MIR →
  LLVM bitcode → pinned Clang object → linked native executable, with a MIR interpreter as the
  differential oracle, deterministic encoders and goldens at every artifact, and nine facade-only
  inspector labs. The two-function "Now" milestone and the former "Later" items for MIR lowering
  and the first native program shipped inside it. Grammar freeze lifted; promoted language
  widening slice 1 (bindings, arithmetic, branching) to Now and data types (issue 02) to Next.
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
