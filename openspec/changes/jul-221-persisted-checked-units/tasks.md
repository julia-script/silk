# Tasks

## 1. Strict complete-unit codec

- [ ] 1.1 Define codec/address versions and explicit bounds for bytes, depth, collections, strings,
      byte arrays, numeric forms, tags/fields, and identifier ranges, verified by focused boundary
      and over-limit fixtures.
- [ ] 1.2 Encode and decode complete checked units with stable declaration/artifact dictionaries,
      current resolution, and no stored source spans or positional declaration authority.
- [ ] 1.3 Validate body ownership, hidden-parent relationships, tables, diagnostics, provenance, and
      every internal reference before publishing any decoded unit; delete the trusting cast decoder.

## 2. Persisted semantic envelopes

- [ ] 2.1 Define one canonical envelope containing exact CheckBody descriptor/key, result
      fingerprint, ordered observations, compiler semantic identity, complete unit, versions, and
      integrity digest.
- [ ] 2.2 Strictly decode descriptors and ordered query/input observations, rejecting unknown
      families/schemas, mismatched addresses, malformed fingerprints, and partial manifests.

## 3. Shared-validator integration

- [ ] 3.1 Add explicit Storage-backed semantic persistence configuration and demand-driven CheckBody
      candidate lookup after current HIR/selection/header preparation.
- [ ] 3.2 Offer decoded candidates to the existing recursive validator, executing current child
      providers for missing records and presenting only admitted complete units against current
      anchors and spans.
- [ ] 3.3 Publish eligible completed CheckBody records after execution; bypass reads, reuse, and
      publication for forced-fresh, cache-disabled, aborted, and cyclic demands.
- [ ] 3.4 Record lookup, validation, reuse, recomputation, rejection/failure, and publication through
      structural counters/reports/traces without adding a second admission authority.

## 4. Restart and failure evidence

- [ ] 4.1 Add one shared restart harness proving unchanged/unrelated reuse and invalidation for body
      edits, insertions, removal/rename, missing-member repair, selection, configuration, and target
      changes.
- [ ] 4.2 Prove complete hidden bodies, eligible rejected units, and all current diagnostic spans
      survive restart or recompute as one unit.
- [ ] 4.3 Prove missing, stale, incompatible, corrupt, oversized, and reference-invalid records
      recompute safely, while typed read/write failure, interruption, and defects follow their
      distinct policies.
- [ ] 4.4 Compare cached and forced-fresh facts/diagnostics and assert provider counters rather than
      timing, native execution, or per-feature determinism.

## 5. Coherence and documentation

- [ ] 5.1 Update compiler architecture/API/trace documentation and the D2 roadmap gate to state that
      current HIR and headers may be rebuilt before checked-unit admission; do not claim C1/C2
      persistence delivered.
- [ ] 5.2 Delete superseded single-body codec and persistence paths, update every caller/export, and
      verify strict OpenSpec validation plus compiler typechecking.
