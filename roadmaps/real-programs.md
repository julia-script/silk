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

**Current objective:** use complete programs to shape a small, coherent language — measured by the
first real Silk lexer producing owned tokens from runtime-sized borrowed input across all engines
and leaving categorized, actionable evidence about every wall it encounters.

## Column rules

- **Now** — problem validated, solution shaped, actively worked or next up. Committed.
- **Next** — problem chosen and understood; solution still in discovery. Planned, not promised.
- **Later** — problem worth solving, no solution chosen. Options, not a queue.

## Now

### Exercise the language with a real lexer

- **Problem:** Small algorithms proved individual capabilities, but they did not sustain a larger
  stateful transformation from runtime-sized borrowed bytes into an owned, growing result. We need
  that pressure without turning a compiler-shaped example into a commitment to self-hosting.
- **Outcome & done-when:** Ordinary Silk source tokenizes representative valid and invalid Silk
  inputs into owned token records, agrees with the canonical TypeScript lexer on token kinds,
  spans, and diagnostics, and runs with deterministic allocation and cleanup across evaluator,
  native, and WebAssembly execution. A checked-in findings report categorizes every discovered gap.
- **Boundary:** No compiler-known token or lexer primitive, no filesystem dependency, no parser
  port, and no claim that the Silk lexer replaces the TypeScript implementation.
- **Status:** active — proposal and implementation loop starting.
- **Appetite:** one focused change; repair only blockers that are both small and clearly general.

## Next

No follow-up program is preselected. The lexer findings decide whether to repair a general seam or
choose another familiar workload that applies different pressure.

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

- 2026-08-09: Completed and archived the six-part foundation: `ship-stdlib-sources`,
  `complete-integer-scalars`, `add-floating-point-scalars`, `add-static-text`,
  `add-standard-streams`, and `build-algorithm-examples`.
- 2026-08-09: Closed the algorithm frontiers through allocation-pressure BFS, indexed static byte
  views, bounded runtime recursion, deterministic transcendental float operations, and effectful
  algorithm entry points. Game of Life, Sieve, matrix multiplication, CRC-32, BFS, quicksort, and
  FFT are executable across supported engines.
- 2026-08-09: Started the lexer as a language-pressure exercise. Self-hosting and a parser port are
  explicitly not implied by this step.
- 2026-08-09: Completed `ship-stdlib-sources` and moved active implementation to
  `complete-integer-scalars`.
- 2026-08-09: Created by splitting the oversized `bootstrap-real-programs` proposal into six
  independently implementable changes and selecting canonical stdlib source as the first seam.
