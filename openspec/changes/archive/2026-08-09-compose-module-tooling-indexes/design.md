## Context

See `proposal.md` for motivation. Project analysis already publishes one immutable semantic artifact
per module and shares it only when semantic invalidation proves the module reusable. Frontend tooling
currently consumes the complete elaboration map and builds two whole-project indexes every time.
Semantic occurrences already contain a module-index layer, and anonymous expressions are already
grouped by module, but neither grouping is an owned reusable artifact.

An occurrence currently embeds its resolved declaration location. That is safe in a freshly built
project but prevents blindly sharing an importer index when an equal dependency contract moves to a
different source span. Composition must therefore distinguish stable occurrence identity from the
current project's presentation location.

## Goals / Non-Goals

**Goals:**

- Make tooling work proportional to modules receiving new semantic artifacts.
- Preserve exact module-artifact identity for proven reusable modules.
- Keep every root view on one immutable, complete, current-project query surface.
- Preserve current definition locations even when the referencing module index is reused.
- Expose exact reuse counters without timing-based correctness tests.

**Non-Goals:**

- Incremental declaration collection, name resolution, surface construction, or diagnostic merging.
- Mutable caches, background index mutation, persistence across processes, or cross-workspace reuse.
- Partial reuse within a module whose semantic artifact was recomputed.
- Changing hover, completion, definition, or inlay-hint query semantics.

## Decisions

### 1. A module tooling actor owns both local indexes

Add one immutable module tooling value containing the canonical module name, its exact semantic input
artifact, one semantic-occurrence module index, and one anonymous-expression array. Keeping the pair
together makes the reuse guard inspectable and prevents the two editor indexes from drifting across
revisions.

The alternative was two independent maps owned only by the project tooling builder. That would
permit finer-grained reuse but duplicate lifecycle and validation logic for facts produced from the
same semantic input.

### 2. Exact semantic artifact identity is the reuse key

Project tooling accepts an optional prior module-tooling map. A prior artifact is retained only when
its module name matches and its semantic input is the exact current module semantic artifact. A
missing or mismatched artifact is recomputed conservatively. This reuses the semantic invalidation
decision already enforced by the project pipeline rather than implementing another invalidation
oracle.

The alternative was to consult invalidation classifications or compare structural encodings again.
Those checks are less direct: exact semantic sharing is both necessary and sufficient at this layer.

### 3. Semantic occurrences separate reusable module indexes from project locations

Occurrence module indexes remain immutable and reusable. Project composition builds a small current
declaration-location registry keyed by canonical semantic identity from the current set of module
indexes. Point and range queries enrich their selected occurrences through that registry before
returning them. Thus a reused importer contributes no rescanning work, while its external reference
resolves to the dependency artifact's current location.

The alternative was to rewrite every reused occurrence during composition. That would avoid a
registry but would forfeit exact module-index sharing and make composition proportional to every
occurrence rather than only declaration-bearing entries.

### 4. Project indexes are shallow immutable compositions

The project semantic-occurrence index owns the current module-index map and location registry. The
anonymous-expression index is a shallow map from module name to each module artifact's frozen array.
Root views share both compositions. Project analysis retains the module-tooling map as the only
predecessor basis for the next accepted revision.

The alternative was lazy query-time fallback from current to predecessor projects. That would make
one published project depend on revision history and complicate stale-revision safety.

### 5. Both tooling phases report module reuse counters

The existing semantic-occurrence and anonymous-expression phase names remain. Each observation gets
the standard module reuse counters, with inputs equal to current modules and outputs retaining their
existing entry-count meaning. Tests assert counters and object identity; elapsed time remains
observational only.

## Risks / Trade-offs

- [A location registry adds one composition pass] → Restrict the pass to declaration-bearing
  occurrences and preserve module occurrence arrays exactly; this is smaller than rescanning syntax
  and semantic trees.
- [A future occurrence kind may lack a declaration occurrence in current modules] → Queries omit a
  location instead of retaining a predecessor fallback, and focused tests cover every current
  identity family used by navigation.
- [Tooling reuse could diverge from semantics reuse] → Store the exact semantic artifact on each
  module tooling value and require identity equality at the single construction boundary.
- [Public data-model changes are breaking] → The project is pre-release and forbids compatibility
  shims; update exports and all in-repo callers atomically.

## Migration Plan

Implement module construction and project composition first, then update fresh analysis and project
analysis to use the same tooling boundary. Add identity, counter, navigation, stale-revision, and
determinism tests; run compiler/LSP checks and the release-candidate validator. Sync the delta specs,
archive the change, and merge the verified branch. Rollback is the single feature commit.
