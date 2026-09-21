# Proposal

## Why

Semantic query sessions currently memoize closure-backed requests only within one revision. Reusing
those records across editor revisions would retain obsolete declarations and caller-specific
diagnostics, so header and name readers need stable descriptors, owned projections, and one shared
dependency validator before cross-revision reuse is safe.

## What Changes

- Replace closure/string semantic requests and ad-hoc observations with closed typed descriptors,
  provider-owned dispatch, canonical addresses, and fingerprinted query/input reads.
- Add bounded previous/current snapshots whose ordered validator stops at the first changed or
  unavailable dependency, executes fresh, and cuts off downstream execution when the result
  projection remains equal.
- Project header, name, import-selection, alias, bound, and header-conformance answers into immutable
  revision-independent content while presenting current declarations, anchors, spans, and
  caller-dependent diagnostics separately.
- Repair qualified and associated lookup dependency recording so every observation has a real
  provider or leaf reader; remove synthetic query keys and closure compatibility paths.
- Add structural counters, forced-fresh execution, abort/retry behavior, bounded snapshot ownership,
  and revision fixtures for negative results, branch replacement, cycles, recovery, and diagnostics.
- Update the compiler architecture and semantic query documentation for Milestone D1a.

## Capabilities

### New Capabilities

- `semantic-revision-validation`: Reconstructible semantic query descriptors, immutable projections,
  ordered dependency validation, result-fingerprint cutoffs, and current presentation across
  revisions.

### Modified Capabilities

- `bootstrap-name-resolution`: Qualified, associated, missing, conflicting, visibility, and import
  selection results become location-independent reusable semantic projections with current-site
  presentation.
- `bootstrap-declaration-index`: Completed header components expose stable observable projections and
  leaf fingerprints without changing eager declaration completion or cycle semantics.

## Impact

The change affects `SemanticQuery`, `Semantic`, name resolution, declaration/header projections,
frontend session construction, analysis/editor reuse, focused invalidation tests, OpenSpec contracts,
and compiler architecture documentation. It does not make header construction lazy, persist results,
or migrate checked bodies and evaluation; those belong to JUL-218 and JUL-221.
