# Roadmap — Real programs

> Direction, not commitment — Now is committed; Next is planned; Later is exploration.
> Only Now items may be promised to anyone. This document changes as we learn.
> Last reviewed: 2026-08-09 · Review cadence: after each OpenSpec archive
> Scope: real-programs initiative — part of [the project roadmap](project.md)

## Vision

Silk's language surface should be judged by recognizable programs rather than compiler-shaped
snippets alone. This initiative fills the smallest foundational gaps—scalar values, visible library
source, static text, and output—then uses familiar algorithms to reveal the next real constraints
without pretending the bootstrap language is already complete.

**Current objective:** repair the native composite-return defect exposed by the completed stack VM,
measured by an engine-independent minimal reproduction and correct cleanup of multiple generic
affine fields without changing their public representation.

## Column rules

- **Now** — problem validated, solution shaped, actively worked or next up. Committed.
- **Next** — problem chosen and understood; solution still in discovery. Planned, not promised.
- **Later** — problem worth solving, no solution chosen. Options, not a queue.

## Now

### Exercise a bounded stack bytecode VM

**Status: complete (2026-08-09).** An ordinary Silk VM matches a TypeScript oracle over arithmetic,
branches, invalid bytecode, stack bounds, and bounded loops. A single ordered event vector allocates
and rolls back deterministically across all three engines. The findings independently confirm named
typed values and shared Vector reads while exposing three narrower compiler defects. Fixed operand
capacity remains honest and visible; no production VM, VM intrinsic, or self-hosting sequence was
introduced.

### Make exact integer context survive enclosing result expectations

**Status: complete (2026-08-09).** Direct, explicit-generic, and piped calls already selected exact
integer literals from concrete parameter types. The actual failure was `bool` from a return context
preventing `byte == 13` from refining `13` to the known `u8` operand type. The repair preserves
unresolved generic inference, unconstrained `i32` defaults, and rejection of already-typed mixed
integers. The lexer now removes its byte-to-`i32` classifier boundary with full engine and
determinism parity.

## Next

Characterize and repair the native flattened-ABI defect for composite returns containing multiple
generic affine fields. Reassess typed constants and shared Vector reads after that focused repair;
do not automatically port another compiler module.

## Later

- Choose the owning `string` model — why it matters: target-native JS strings may conflict with a
  mutable UTF-8 layout; revisit after static text and a real embedding host provide evidence.
- Add structured `Effect.log` through a replaceable Logger — why it matters: logs may route to
  standard streams, OpenTelemetry, tests, memory, or fan-out.
- Add general default-overridable service providers — why it matters: Logger should not receive a
  unique ambient-dependency exception; revisit after several explicit services exist.
- Shape Stream/Sink abstractions from actual I/O consumers — why it matters: process streams are a
  boundary, not yet evidence for the complete streaming model.

## Maintenance budget

Reserve approximately 20% of initiative capacity for migrating existing fixtures, goldens, docs,
and editor tooling together with the language surface so no second vocabulary survives.

- Keep evaluator/native/Wasm differential evidence complete at every scalar-family boundary.
- Keep package-content tests and generated stdlib bytes synchronized with canonical `.silk` files.

## Not doing

- Uppercase primitive compatibility aliases — the project is unreleased and should converge once.
- A growable owning String in this initiative — static literal/output needs do not settle its model.
- `Effect.log` as stdout sugar — logging and raw process bytes have different routing semantics.
- Ambient `StandardStreams` or Logger — defaults will be a general service feature later.
- Algorithm-specific intrinsics, fake byte wrappers, or precomputed answers — frontier status is
  preferable to dishonest executability.

## Open questions

- Does real static-text use favor a target-selected semantic string representation or a text service?
- Which lexer findings are general enough to justify a language or standard-library change rather
  than remaining local ergonomics?

## Changelog

- 2026-08-09: Completed `exercise-language-with-stack-vm`. Ten differential corpus cases, exact
  ordered observations, evaluator/native/Wasm parity, allocation-failure sweeps, and fresh-process
  determinism pass. The VM confirmed named-value and shared-read pressure and selected the native
  multi-vector aggregate return defect as the next focused repair.
- 2026-08-09: Selected a bounded stack bytecode VM as the second language-pressure program so
  closed numeric codes, Vector reads, allocation behavior, and cost findings receive independent
  evidence before any general feature is proposed.
- 2026-08-09: Completed `fix-contextual-integer-call-literals`. Focused tests corrected the initial
  call-boundary diagnosis and repaired homogeneous operand refinement under enclosing Boolean
  result context. The lexer now compares `u8` values and literals directly across all engines.
- 2026-08-09: Completed and archived the six-part foundation: `ship-stdlib-sources`,
  `complete-integer-scalars`, `add-floating-point-scalars`, `add-static-text`,
  `add-standard-streams`, and `build-algorithm-examples`.
- 2026-08-09: Closed the algorithm frontiers through allocation-pressure BFS, indexed static byte
  views, bounded runtime recursion, deterministic transcendental float operations, and effectful
  algorithm entry points. Game of Life, Sieve, matrix multiplication, CRC-32, BFS, quicksort, and
  FFT are executable across supported engines.
- 2026-08-09: Started the lexer as a language-pressure exercise. Self-hosting and a parser port are
  explicitly not implied by this step.
- 2026-08-09: Completed and archived `exercise-language-with-silk-lexer`: exact TypeScript
  differential parity across all 67 token kinds, invalid diagnostics, owned token/diagnostic
  vectors, four failure ordinals, evaluator/native/Wasm parity, and deterministic artifacts. The
  findings selected ordinary-call contextual integer literals as the next repair.
- 2026-08-09: Completed `ship-stdlib-sources` and moved active implementation to
  `complete-integer-scalars`.
- 2026-08-09: Created by splitting the oversized `bootstrap-real-programs` proposal into six
  independently implementable changes and selecting canonical stdlib source as the first seam.
