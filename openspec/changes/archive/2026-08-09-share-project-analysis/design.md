## Context

See `proposal.md` for motivation. After `separate-frontend-analysis`, ordinary LSP work stops after
frontend analysis, but `ProjectSession` still loops over synchronized documents and calls
`Workspace.analyze` once per root. Each call builds a separate closure and frontend snapshot even
when roots overlap. The scheduler's one-worker, latest-wins, version-matched, atomic-commit behavior
is a constraint, as is compiler ownership of all semantic facts.

The compiler frontend currently accepts one `ModuleClosure.CompilationRequest`. Closure loading is
the only frontend operation that inherently depends on root count; declaration collection, name
resolution, elaboration, ownership, and tooling indexes can operate over a deterministic union of
loaded modules. Runtime instance discovery remains root-specific and is outside this editor path.

## Goals / Non-Goals

**Goals:**

- Analyze the union closure of all captured project roots once per accepted LSP revision.
- Preserve a root identity for each analyzed document without copying expensive frontend artifacts.
- Make structural sharing and single execution directly testable through references and phase data.
- Keep the existing atomic scheduling boundary around the complete analyzed-document map.

**Non-Goals:**

- Reusing artifacts between different project revisions.
- Dependency-aware or public-surface-aware invalidation.
- Sharing runtime realizations between roots.
- Incremental parsing, persistent green/red trees, or cross-revision syntax correspondence.
- Changing protocol results, debounce policy, filesystem watching, or overlay precedence.

## Decisions

### 1. Add a compiler-owned `ProjectAnalysis` actor

`ProjectAnalysis.make(roots)` is the public multi-root frontend entry point. It returns one immutable
project value containing canonical roots, union-closure facts, tooling indexes, and one phase report.
`ProjectAnalysis.view(project, rootModule)` returns the `Analysis.FrontendSnapshot` view for a
requested root.

The actor is deliberately deeper than an LSP cache: the compiler defines union-closure semantics,
fact sharing, diagnostics, and observations; the LSP only supplies synchronized roots and adapts
views to protocol documents.

Alternatives considered:

- Caching separate `Analysis.make` results inside the LSP was rejected because overlapping roots
  would still compute duplicate artifacts and the LSP would own compiler invalidation semantics.
- Picking one open document as the sole analysis root was rejected because another open document can
  be unreachable from it and must remain independently queryable.
- Adding a multi-root option directly to `Analysis.make` was rejected because a project revision and
  a single-root snapshot are distinct actors with different construction and lookup operations.

### 2. Generalize closure loading around an ordered root set

`ModuleClosure` gains one shared loading implementation seeded by a deterministically sorted,
deduplicated root map. Existing single-root `load` delegates to it. Every canonical module is
resolved, parsed, and diagnosed once because the frontier and resolution maps are shared.

The project closure retains all requested root identities. A root view creates only a shallow
closure shell selecting `rootModule`; its module array, source map, cycles, resolution failures, and
module artifacts remain the exact project-owned values. Frontend facts and indexes are project-wide
and module-qualified, so unrelated roots do not become implicitly importable.

Alternatives considered:

- Merging completed single-root closures was rejected because parsing and import resolution would
  already have been duplicated before the merge.
- Filtering and copying each root's reachable subclosure was rejected for this stage because LSP
  queries are module-qualified and project-wide immutable facts provide simpler, directly shared
  views. Root-specific runtime reachability remains deferred to explicit realization.

### 3. Execute tooling indexes once at project scope

The pipeline constructs the union frontend once, then `ProjectAnalysis` builds semantic occurrences
and anonymous-expression indexes once. Every root view references those indexes and the same phase
report. Closure-phase input count records the number of roots; later counts record union modules or
facts. No view-creation phase is appended because view derivation is a shallow immutable projection,
not compiler work.

Alternatives considered:

- Building tooling indexes per document was rejected because both indexes are keyed by canonical
  module identity and would reproduce the exact multiplication this change removes.
- Adding a process-global cache was rejected because it would introduce cross-project lifetime,
  eviction, and revision-coherence questions before dependency invalidation exists.

### 4. Make `ProjectSession` analyze one captured revision value

`ProjectSession.Options.analyze` changes from a per-document callback to a callback receiving the
captured document array and returning the complete URI-keyed analyzed-document map. The worker calls
it once, verifies the captured revision is still current, atomically replaces `committed`, publishes
the completed values, and resolves exact-version waiters.

`Workspace.analyzeProject` builds root `SourceFile`s from every captured document, supplies one
overlay resolver containing all synchronized sources, constructs `ProjectAnalysis`, and adapts one
root view per document. URI mapping is computed once from the shared source map and attached by
reference to every analyzed document.

Alternatives considered:

- Keeping the callback per document and passing an implicit mutable cache was rejected because the
  scheduler could not enforce one coherent project result or reason clearly about cache lifetime.
- Publishing views as soon as each is derived was rejected because it would weaken the existing
  atomic replacement rule for no meaningful latency gain; view derivation is shallow.

## Risks / Trade-offs

- **[Risk]** Project-wide indexes retain modules reachable from any open root in every view. →
  **Mitigation:** all frontend queries used by the LSP are module-qualified, resolution remains
  import-scoped, and tests cover unrelated roots with colliding local spellings.
- **[Risk]** Duplicate canonical roots with different bytes would make project input ambiguous. →
  **Mitigation:** reject conflicting duplicates at the compiler boundary; `ProjectSession` already
  stores one current document per URI/module identity.
- **[Risk]** A failure while building one root blocks the entire atomic map. → **Mitigation:** source
  and semantic damage remains recoverable data as today; only impossible input-contract violations
  abort construction.
- **[Trade-off]** This removes within-revision root multiplication but still recomputes the union on
  every accepted edit. That cleanly isolates cross-revision reuse as the next cache/invalidation
  change without mixing lifetime policies into this one.

## Migration Plan

1. Generalize closure and pipeline construction for deterministic multi-root frontend analysis.
2. Add `ProjectAnalysis` with shared tooling indexes, observations, and root-view tests.
3. Change the ProjectSession callback and atomic commit path to accept one complete project result.
4. Move Workspace and Server adaptation to one project analysis per captured revision.
5. Run the full repository and release-candidate gates, then sync and archive the change.

Rollback is a source revert to per-document frontend construction; no persisted format or user data
is introduced.
