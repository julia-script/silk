## Why

Adjacent accepted project revisions already compute a sound module invalidation plan, but still
elaborate and ownership-check every module. The next safe performance layer is to turn reusable
classifications into structural sharing without allowing old project-wide indexes or stale source
locations to leak into the new immutable project.

## What Changes

- Introduce one compiler-owned immutable semantic artifact per module, owning that module's
  elaboration/HIR and ownership facts independently of a closure-wide declaration index.
- Use the existing semantic invalidation oracle inside project frontend construction so reusable
  module artifacts are retained by reference and only invalidated modules execute elaboration and
  ownership analysis.
- Keep declaration collection, declaration completion, name resolution, semantic surfaces, merged
  diagnostics, and project tooling coherent for the complete current closure; this change does not
  yet make tooling indexes module-composable.
- Resolve source-backed editor locations through the current declaration index so structurally
  shared semantic facts cannot publish predecessor spans for a changed dependency.
- **BREAKING** Remove the closure-wide declaration index from `Elaboration.Result`; ownership takes
  its required current index explicitly, and project views become a distinct non-realizable type.
- Extend phase reports with deterministic module reuse/recomputation counters and retain the
  committed-predecessor, SCC invalidation, immutable snapshot, and atomic LSP commit contracts.

## Capabilities

### New Capabilities

- `bootstrap-module-semantics`: Defines independently owned immutable module semantic artifacts and
  their safe structural-sharing contract across adjacent project revisions.

### Modified Capabilities

- `bootstrap-project-analysis`: Turns semantic invalidation evidence into actual module-level
  elaboration/ownership reuse while preserving one coherent current project.
- `bootstrap-analysis-facade`: Distinguishes query-compatible project views from single-root
  frontend snapshots that may be passed to runtime realization.

## Impact

This changes compiler frontend orchestration, elaboration and ownership APIs, project analysis,
semantic occurrence location resolution, phase-report counters, LSP snapshot typing, and focused
compiler/LSP tests. Batch `Analysis.make` remains history-independent, project tooling indexes are
still rebuilt globally, no mutable cache is introduced, and no parser or runtime-realization design
is changed.
