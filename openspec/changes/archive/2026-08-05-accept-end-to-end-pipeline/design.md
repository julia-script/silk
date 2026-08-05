# Design — accept-end-to-end-pipeline

## Context

See proposal.md — Why. Every phase and service exists; this change proves the spine: one driver
path to a running executable, the differential harness and determinism gates running
continuously in the suite CI enforces, per-phase reporting, and the pipeline-overview lab. After
it, the grammar freeze lifts.

## Goals / Non-Goals

**Goals**

- `Driver.ts` (Node-only deep import): `compile(request)` orchestrating every phase and both
  services itself, with closed outcomes (`Compiled` / `NoEntry` / `Failed {stage, provenance}`)
  and a per-phase report.
- Host-derived target layout (`hostLayout`) so objects and links match the machine running the
  driver — fixing the pinned-triple assumption the toolchain tests carried.
- The differential harness over the shared corpus: interpreter vs native execution, divergence
  fails naming the program; trap programs must terminate abnormally; recursion programs
  compile-only.
- The pipeline-overview lab; the roadmap's freeze note lifted and status flipped to done.

**Non-Goals**

- No new CI jobs: `pnpm check` already runs the whole suite in CI, so gates land as tests.
- No benchmark protocol, reference machines, or stored measurements — issue 09 owns those; this
  change provides the reporting substrate.
- No new language surface.

## Decisions

1. **The driver runs phases individually rather than through `Analysis.make`** — it is the one
   place that times and counts each phase, and the facade remains the tooling surface. Phase
   order and outputs are identical to the snapshot's by construction (same phase functions).

2. **Memory totals are engine-heap observations** (`process.memoryUsage().heapUsed` after each
   phase): the honest bootstrap approximation of allocator-backed totals until self-hosted
   allocators exist. Reports are observability data and exempt from byte-identity.

3. **`hostLayout()` derives the triple from the host** (darwin/linux × arm64/x64), used by the
   driver and the toolchain tests; the facade's fixed default layout remains for browser-side
   inspection artifacts where no process runs.

4. **Differential expectations per corpus outcome**: `Completes` → native exit status equals the
   result (all corpus results fit in an exit byte); `Trap` → abnormal termination (signal or
   non-zero status); `RecursiveCycle` → the program must still compile, but is not executed —
   native unbounded recursion is a crash, not a comparable outcome, and the interpreter's bound
   is deliberately not a language-wide policy.

5. **The overview lab times one facade snapshot build and derives per-phase counts from it**
   (modules, headers, functions, instances, MIR functions, per-phase diagnostic counts), linking
   each row to the phase's dedicated lab and listing the planned native stages — the browser
   spawns nothing.

## Risks / Trade-offs

- [Toolchain-dependent tests in CI] → ubuntu-latest ships `/usr/bin/clang`; the host layout makes
  objects and links native to the runner. If a runner ever lacks Clang, the failure names the
  pinned path explicitly.
- [Exit-status comparison is 8-bit] → Corpus results stay in range by construction; issue 07 owns
  richer result delivery.

## Migration Plan

1. Land `hostLayout` + `Driver.ts` + driver tests; re-point toolchain tests to the host layout.
2. Land the differential harness test over the corpus.
3. Add the overview lab; lift the roadmap freeze and flip its status.
4. Rollback is git-revert.

## Open Questions

None — this closes the realignment.
