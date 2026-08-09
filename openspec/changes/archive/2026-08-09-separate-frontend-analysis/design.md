## Context

`Analysis.make` and `Driver.compile` independently orchestrate closure loading, declaration analysis,
name resolution, elaboration, ownership, instance discovery, target layout, and MIR lowering.
`Analysis.make` also constructs tooling indexes and always realizes runtime facts. The language server
calls it once for each synchronized document root, although its protocol features consume only
frontend facts. See `proposal.md` for motivation and the delta specs for observable requirements.

The existing immutable snapshot and latest-wins project commit rules are constraints. The compiler
must continue to own semantic truth, malformed source must remain queryable, the driver must retain
its early rejection and toolchain policies, and browser analysis cannot depend on Node-only memory
instrumentation.

## Goals / Non-Goals

**Goals:**

- Put compiler phase progression behind one deep internal module with a small interface: construct
  frontend facts, derive recoverable runtime facts, or prepare gated runtime facts for Driver.
- Make the type-level distinction between frontend and realized snapshots prevent accidental runtime
  work in editor consumers.
- Preserve exact frontend facts by value across realization.
- Give Analysis and Driver one canonical phase vocabulary and ordering while allowing the driver to
  append backend and toolchain phases.
- Leave seams that later project-level reuse can deepen without changing LSP semantics again.

**Non-Goals:**

- Module caching, dependency-aware invalidation, shared project-revision stores, or cache-hit metrics.
- Incremental parsing, persistent syntax trees, or cross-revision syntax correspondence.
- Changing `ProjectSession` scheduling, overlay precedence, or atomic commit behavior.
- Making the driver depend on the tooling facade.

## Decisions

### 1. Use an internal `Pipeline` actor as the compiler phase seam

`Pipeline` owns three policy-shaped operations over the same compiler phases:

```text
Pipeline.frontend(request) -> immutable Frontend
Pipeline.realize(frontend, target) -> immutable Realization
Pipeline.prepare(frontend, backend, target) -> closed Driver Preparation
```

`Frontend` retains closure, declaration index, name resolution, elaboration results, ownership,
merged frontend diagnostics, and phase observations. `Realization` retains instances, target,
layout catalog, layout plan, MIR, merged diagnostics, and its appended observations.

Analysis adapts the recoverable values into its public snapshots and adds tooling-only indexes.
Driver consumes the same frontend and phase implementations through `prepare`, whose closed result
preserves early source-diagnostic, missing-entry, target, and backend-compatibility gates before
backend and artifact finalization. This keeps the module deep: callers select a policy-shaped entry
point and do not orchestrate individual phases.

Alternatives considered:

- A phase-by-phase public builder was rejected because it exposes ordering invariants and creates a
  shallow interface every caller can misuse.
- Making Driver call Analysis was rejected because it couples batch compilation policy to the tooling
  facade and makes recoverable snapshot behavior the driver's orchestration contract.
- Parameterizing `realize` with rejection callbacks was rejected because it would expose phase-order
  policy through a shallow configuration surface; a closed Driver preparation result makes the
  distinct policy explicit without duplicating orchestration in Driver.
- A boolean `frontendOnly` option on the existing constructor was rejected because the return type
  would contain runtime fields whose availability depends on a flag.

### 2. Model realization as an explicit immutable derivation

`Analysis.make` returns `FrontendSnapshot`. `Analysis.realize` accepts that snapshot and an optional
target selection and returns the fully realized `Snapshot`. The realized snapshot contains the same
frontend facts by value plus runtime facts; realization never mutates or memoizes onto its input.
Frontend query functions accept `FrontendSnapshot`, while runtime-only queries accept `Snapshot`.

This deliberately breaks eager construction. Call sites must reveal whether they need runtime facts.
The explicitly named `makeRealized` and `ofSourceRealized` conveniences compose construction with
`realize` for runtime-heavy consumers without making `make` eager or weakening snapshot types.
`Analysis.evaluate` and backend emission remain facade operations over realized snapshots.

Alternatives considered:

- Hidden lazy fields were rejected because synchronous query access would require internal mutation
  or unstable promise-shaped fields and would make execution cost invisible to callers.
- Returning explicit `NotRequested` values in every frontend snapshot was rejected because runtime
  fields would remain in the editor-facing interface and invalid states would be representable.

### 3. Share phase data, not one environment-specific recorder

The pipeline defines the canonical phase names and immutable observation shape. Its phase runner
records elapsed time plus input, output, and diagnostic counts. A caller may supply a synchronous
heap-total probe; Driver supplies the Node engine heap total and browser-compatible Analysis omits it.
Driver uses the same phase helper for its later backend/toolchain entries.

Tooling-only index construction is appended by Analysis using the same observation shape. Timing and
heap values are excluded from deterministic encodings and equality gates; phase names, order, and
counts are testable.

Alternatives considered:

- Effect `Clock` was not selected for the synchronous compiler inner phases because it would turn
  every pure phase into an effect solely for timing. The surrounding public operations remain
  Effect-native where source resolution or backend work is effectful.
- Retaining Driver's private report vocabulary was rejected because later cache/reuse measurements
  would otherwise need two incompatible observability paths.

### 4. Keep LSP scheduling unchanged and change only its analyzed value

`Workspace.analyze`, `ProjectSession.AnalyzedDocument`, and document feature adapters use
`FrontendSnapshot`. `ProjectSession` still analyzes current roots in its existing order and commits
one complete map only when the captured revision remains current. This change therefore removes
runtime work without attempting project-wide sharing yet.

## Risks / Trade-offs

- **[Risk]** Snapshot type migration touches many compiler tests and tooling consumers. → **Mitigation:**
  convert frontend-only consumers first, add a small test helper for tests that truly need realized
  snapshots, and let type errors identify accidental runtime dependencies.
- **[Risk]** Pipeline extraction could change diagnostic or phase ordering. → **Mitigation:** preserve
  existing algorithms by moving orchestration without rewriting phases, then compare Analysis and
  Driver outputs on identical fixtures.
- **[Risk]** Analysis timing based on short synchronous phases can be noisy. → **Mitigation:** assert
  phase presence, order, and counts rather than wall-clock thresholds; use measurements as baseline
  observations, not pass/fail performance budgets.
- **[Trade-off]** Frontend snapshots and realized snapshots duplicate a shallow object shell. →
  Runtime realization reuses the immutable frontend values, so the shell buys a clear type-level
  interface without duplicating compiler artifacts.

## Migration Plan

1. Introduce the canonical phase observation and internal Pipeline actors with equivalence tests.
2. Split Analysis construction from realization and migrate runtime-dependent compiler tests and
   consumers explicitly.
3. Move Driver frontend and realization orchestration onto Pipeline while retaining its existing
   outcome gates and artifact phases.
4. Change LSP snapshot types to frontend-only and add phase-exclusion coverage alongside existing
   scheduling tests.
5. Run repository checks and release-candidate verification because compiler package interfaces and
   exports change.

Rollback is a normal source revert before the next incremental-analysis change; no persisted user
data or migration state is introduced.
