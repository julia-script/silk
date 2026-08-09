## Why

Every accepted language-server revision currently rebuilds instance discovery, target layout, and
MIR for each open compilation root even though editor features consume only frontend semantic
facts. Analysis and the batch driver also duplicate the same frontend phase progression, making
performance instrumentation and later reuse vulnerable to drift between two orchestration paths.

## What Changes

- Introduce one compiler-owned internal pipeline module that constructs immutable frontend facts
  once and can explicitly derive target/runtime facts from them.
- **BREAKING**: distinguish an immutable frontend analysis snapshot from a fully realized analysis
  snapshot instead of making every `Analysis.make` call eagerly produce instances, layout, and MIR.
- Make runtime realization an explicit immutable derivation; frontend snapshots remain coherent and
  never gain hidden mutable lazy state.
- Make the language server retain frontend snapshots for diagnostics, hover, completion, navigation,
  symbols, inlay hints, and formatting without executing instance discovery, target layout, or MIR
  lowering.
- Make the batch driver consume the same compiler pipeline while preserving its rejection gates,
  backend/toolchain policy, closed outcomes, and executed-phase reporting.
- Report frontend and realization phase work through one canonical phase vocabulary so tests can
  prove which phases executed and establish a baseline for later cache/reuse work.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-analysis-facade`: distinguish frontend snapshots from explicitly realized snapshots,
  preserve immutable coherent queries at both levels, and expose executed-phase observations.
- `bootstrap-compiler-driver`: require the driver to consume the canonical compiler pipeline while
  retaining only the phases it actually executes in its report.
- `language-server-synchronization`: require ordinary project analysis to commit coherent frontend
  snapshots without executing runtime realization phases.

## Impact

- Compiler orchestration in `packages/compiler/src/Analysis.ts` and
  `packages/compiler/src/Driver.ts` moves behind a new concept-oriented pipeline actor.
- Analysis snapshot types and construction/realization operations change; compiler, CLI, docs, LSP,
  and tests that currently assume eager runtime fields must select the appropriate snapshot level.
- The LSP scheduling and atomic revision model remain intact; only the computation used to produce
  each committed document snapshot changes.
- No cache, dependency invalidation store, persistent syntax tree, or edit-stable cross-revision
  syntax correspondence is introduced by this change.
