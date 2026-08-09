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

**Current objective:** make the first familiar algorithm set executable and inspectable — measured
by cross-engine agreement for Game of Life, Sieve, matrix multiplication, and CRC-32, plus durable
frontier evidence for quicksort and FFT where required.

## Column rules

- **Now** — problem validated, solution shaped, actively worked or next up. Committed.
- **Next** — problem chosen and understood; solution still in discovery. Planned, not promised.
- **Later** — problem worth solving, no solution chosen. Options, not a queue.

## Now

### Make shipped standard-library source real and navigable

- **Problem:** `silk/vector` is embedded in a JavaScript string and the LSP invents a project URI
  for its definitions, so the canonical source neither ships as an inspectable file nor supports
  truthful go-to-definition.
- **Outcome & done-when:** Physical `.silk` files are canonical, generated embedding is verified,
  installed packages include the files, and navigation opens the analyzed toolchain source.
- **Status:** complete (2026-08-09) — source generation, origins, navigation, and packed artifacts verified.
- **Appetite:** one focused change.
- **Links:** change: `ship-stdlib-sources`

### Complete the integer foundation

- **Problem:** Only `I32`, `Usize`, and `Bool` are genuine scalars; source spelling is cumbersome,
  bytes are counterfeited by a nominal wrapper, and indices still use mixed conventions.
- **Outcome & done-when:** Lowercase unit/bottom and the full signed/unsigned integer family have
  exact literals, explicit conversions and operation modes, `usize` indices, and evaluator/LLVM/
  Wasm parity with no uppercase compatibility aliases.
- **Status:** in progress — implementation is now applying the 19-task integer parity plan.
- **Appetite:** one substantial but integer-only change with an internal parity matrix.
- **Links:** change: `complete-integer-scalars` ·
  [accepted type model](../wayfinder/bootstrap-language/issues/02-bootstrap-type-system-and-values.md)

## Next

### Real numeric algorithms need floating-point values

- **Problem:** FFT and many familiar numerical programs cannot be represented without `f32` and
  `f64` or tested honestly without explicit IEEE semantics.
- **Hypothesis:** a focused conservative-float change after the integer catalog will preserve
  backend parity while keeping fast math and a complete math library out of scope.
- **Confidence:** high
- **Assumes:** the scalar catalog and exact conversion boundary survive the integer change — unvalidated
- **Open questions:** which missing math operations FFT records as frontier evidence.
- **Links:** change: `add-floating-point-scalars`

### Observable programs need text bytes and explicit output

- **Problem:** Programs cannot render a board or explain a result, while prematurely defining
  String, Logger, or Streams would entangle several still-open architecture choices.
- **Hypothesis:** static UTF-8/byte literals followed by an explicit `StandardStreams.writeAll`
  service will enable observation without fixing owning String or structured logging semantics.
- **Confidence:** medium
- **Assumes:** static literal storage can remain a non-owning target-neutral view — unvalidated
- **Open questions:** exact private Wasm host import naming; not the source-level service contract.
- **Links:** change: `add-static-text` · change: `add-standard-streams`

### Familiar algorithms become the acceptance surface

- **Problem:** Synthetic fixtures reveal phase bugs but not whether ordinary programs compose
  naturally or which missing capabilities matter to users.
- **Hypothesis:** executable and frontier examples with checked-in evidence will expose useful walls
  without distorting algorithms to satisfy the compiler.
- **Confidence:** high
- **Assumes:** Game of Life, Sieve, matrix multiplication, and CRC-32 fit the shaped foundation — unvalidated
- **Open questions:** whether quicksort needs additional slice behavior and which FFT dependencies remain.
- **Links:** change: `build-algorithm-examples`

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

- Which quicksort and FFT blockers remain after the shaped changes are implemented?
- Does real static-text use favor a target-selected semantic string representation or a text service?

## Changelog

- 2026-08-09: Completed `ship-stdlib-sources` and moved active implementation to
  `complete-integer-scalars`.
- 2026-08-09: Created by splitting the oversized `bootstrap-real-programs` proposal into six
  independently implementable changes and selecting canonical stdlib source as the first seam.
