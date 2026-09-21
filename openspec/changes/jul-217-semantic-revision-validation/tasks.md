# Tasks

## 1. Revision Query Runtime

- [x] 1.1 Replace closure/string requests with closed typed descriptors, canonical addresses, provider dispatch, and typed query/input reads; verify SemanticQuery unit fixtures cover address stability and real readers
- [x] 1.2 Add previous/current snapshots, ordered recursive validation, result fingerprints, branch replacement, bounded retention, and validation/execution/reuse counters; verify focused runtime fixtures cover hits, changed inputs, equal-result cutoffs, missing records, cycles, and abort/retry
- [x] 1.3 Make forced-fresh mode bypass current and previous reuse for root and nested providers; verify fresh and validated structural outputs agree

## 2. Header and Name Providers

- [x] 2.1 Add stable completed-header/component, alias, bound, conformance, namespace, binding-selection, candidate-set, and configuration leaf projections; verify ordinals, positions, and prior revision objects are absent from reusable content
- [x] 2.2 Migrate name, qualified/associated name, and declared-type readers to typed providers with exact positive and negative observations; verify qualifier retargeting, missing-member repair, visibility/conflict/import changes, and candidate additions/removals invalidate only affected work
- [x] 2.3 Separate location-independent lookup outcomes from current diagnostic presentation; verify two rejected uses and moved uses publish diagnostics at every current site

## 3. Integration and Documentation

- [x] 3.1 Transfer one finalized semantic snapshot between analysis revisions and preserve eager legal header cycles/component invalidation; verify unrelated edits reuse readers and merge/split or recovered-byte changes invalidate them
- [x] 3.2 Remove obsolete closure requests, synthetic query keys, and alternate general validation paths, then update public exports, trace labels, OpenSpec contracts, compiler architecture, and focused fixtures
