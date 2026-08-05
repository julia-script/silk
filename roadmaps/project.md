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

**Current objective:** give the bootstrap language compiler-shaped data over the accepted compiler
spine. The next work first standardizes target-aware layout in the compiler, then resolves
cross-module declarations before nominal structs, structural unions, and exhaustive matching grow
through the same inspectable, determinism-gated architecture.

## Column rules

- **Now** — problem validated, solution shaped, actively worked or next up. Committed.
- **Next** — problem chosen and understood; solution still in discovery. Planned, not promised.
- **Later** — problem worth solving, no solution chosen. Options, not a queue.

## Now

### Widen the language, slice 1: bindings, arithmetic, branching

**Status: complete (2026-08-05).** All three changes shipped and archived; see the changelog.

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

### Give the language its data: structs, unions, and matching

**Status: shaped; first change ready to propose.** The semantic choices are pinned in Wayfinder,
including compiler-owned target layout and the concrete import, construction, and matching forms.

- **Problem:** A compiler-shaped program is mostly data manipulation; expressions and branches
  alone cannot express tokens, trees, or facts. The current backend-owned layout seam also hides
  target facts until emission, where multiple backends could repeat or disagree on Silk layout.
- **Outcome & done-when:** the compiler selects a supported target before lowering, computes one
  backend-neutral layout plan after concrete instance discovery, and embeds it in MIR. The language
  then gains cross-module resolution, nominal structs, struct values, normalized structural unions,
  and exhaustive mode-aware matching without allowing a backend to reinterpret their layouts.
- **Sequence:**
  1. `standardize-target-aware-layouts`
  2. `resolve-cross-module-declarations`
  3. `declare-nominal-struct-types`
  4. `construct-and-project-struct-values`
  5. `normalize-structural-unions`
  6. `match-exhaustively`
- **Dependencies:** layout and cross-module resolution both precede struct declarations; struct
  declarations precede construction; construction and layout precede unions; unions precede match.
- **Appetite:** one focused OpenSpec change at a time, followed by implementation, inspection,
  archive, and reassessment before proposing the next ticket.
- **Links:** [type and value decision](../wayfinder/bootstrap-language/issues/02-bootstrap-type-system-and-values.md) ·
  [module decision](../wayfinder/bootstrap-language/issues/04-modules-visibility-and-name-resolution.md) ·
  [pipeline decision](../wayfinder/bootstrap-language/issues/06-bootstrap-compiler-pipeline.md) ·
  [syntax decision](../wayfinder/bootstrap-language/issues/08-prototype-bootstrap-syntax.md)

## Next

No item is promoted yet. Reassess after each data-slice change is implemented and archived; the
six-item Now sequence is intentionally allowed to change when implementation evidence demands it.

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

- 2026-08-05: Promoted the compiler-data slice to Now and shaped six dependency-ordered changes.
  Settled that the compiler is backend-agnostic but target-aware: canonical target and concrete
  layout are computed before MIR lowering and embedded in MIR. Also fixed the bootstrap import,
  nominal construction, union widening, and mode-aware match decisions in Wayfinder.
- 2026-08-05: Shipped and archived `branch-on-boolean-conditions` — slice 1 complete. `Bool` as
  the second scalar (literals, declared types, comparisons, `Bool.not`), `if`/`else` statements
  with brace arms, condition and argument type checking (`SEM0011`/`SEM0012`), arm-scoped
  ownership with per-return and arm-end exits and conservative conditional moves, MIR branch
  diamonds with join blocks and arm drops, exact interpreter branching, and native `icmp`/`zext`
  emission. Six branching corpus programs hold interpreter/native parity arm by arm. The
  language now binds, computes, and decides through every phase of the spine.
- 2026-08-05: Shipped and archived `compute-integer-arithmetic` — slice 1's second change.
  Signed literals with full `I32` range, qualified callees, and the compiler-known `I32` actor
  (`add`/`subtract`/`multiply`/`divide`/`remainder`) as HIR builtin calls lowering to a trapping
  MIR `Binary` operation; the interpreter traps exactly on overflow, division by zero, and
  MIN/-1, and the backend expands to overflow intrinsics plus guarded division branching to trap
  blocks. Six new corpus programs hold interpreter/native parity including native trap behavior.
  Two recorded design deviations: built-ins live in an elaboration table (not the declaration
  index), and the checked expansion is visible at the LLVM level (MIR stays compact). Next:
  `Bool`, comparisons, `if`/`else`.
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
