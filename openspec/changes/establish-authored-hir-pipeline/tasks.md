# Tasks

## 1. A1 — JUL-205 authored representation and encoding

- [x] 1.1 Implement logical owner identities and owner-local anchors, with stable unique-name and distinct conditional/duplicate fixtures.
- [x] 1.2 Implement immutable module-owned text/byte pools and exact literal payloads, with embedded-zero and pre-rounding fixtures.
- [x] 1.3 Implement the complete closed authored HIR vocabulary and separate presentation artifact, with exhaustive syntax-category coverage and source-independent structural fixtures.
- [x] 1.4 Implement versioned canonical header/body encoding and SHA-256 fingerprints, with committed goldens for pool renumbering, presentation independence, body-only edits and explicit recovery.

## 2. A2 — JUL-206 complete local lowering (separate work package)

- [x] 2.1 Extend existing syntax/recovery specification deltas and implement all-form local lowering, preserving inactive arms, static bodies, lexical relationships, exact values and current presentation.
- [x] 2.2 Migrate representative structural source fixtures to the authored boundary, including recovery and source-release evidence; preserve syntax-only formatting.

## 3. A3 — JUL-207 HIR-to-TIR semantics (separate work package)

- [x] 3.1 Carry the authored module and presentation beside every loaded module through the closure and elaboration inputs, and derive body-query implementation/scope keys and hidden anonymous identities from authored content instead of source tokens and byte offsets, preserving the positive/negative reuse witnesses including alpha renaming.
- [x] 3.2 Extend the remaining semantic/reuse specification deltas and migrate declaration, resolution, typing, conformance, static and ownership consumers to authored HIR with explicit context.
- [ ] 3.3 Publish one typed TIR body plus necessary indexed results, remove duplicate executable facts and syntax-backed rebinding, and migrate public/tooling terminology and goldens. The contract, representative artifacts, consumer map and step rationale are in `tir-contract.md`.
  - [x] 3.3.1 Locations: `Location` and value-coordinate `Provenance`; every header and body diagnostic, cause identity and evaluator failure holds a location, never a span; static text provenance names a literal's anchor and value offsets; one publication point in `Frontend` resolves locations through the presented spelling map, with multi-part locations and callee-owned literal parts. Stages after TIR still report in source coordinates until 3.3.4.
  - [ ] 3.3.2 Identities and artifacts: span-derived identities (`BorrowId.callSpan`, `TemporaryOwnerId`, match and site ids, anonymous aggregate and module-condition ordinals, reflected field provenance) rebuilt from anchors; `ArtifactId`, `Application`, `NodeRef`, `EvaluationKey`; declarations, fields and members by id in TIR and cached products; request identity separated from validity, with the reuse policy explicit: a reused rejection is never admitted as checked, validity is canonical authored content rather than source bytes, repair invalidates, healthy owners stay reusable; a hit returns the cached object; aborted builds publish nothing; delete `SemanticRebinding`.
  - [ ] 3.3.3 The TIR schema: node and local ids, origins, resolved operations and explicit conversions on nodes, `evidence`/`causes` references, supplementary tables, field registry and canonical codec; static bodies published; goldens move to the source-free encoding.
  - [ ] 3.3.4 Whole-body consumers onto TIR: lifetime flow, control flow, type outlives, ownership, opaque realization, instances, then occurrences, completion, type hints, inspector and the LSP/docgen callers.
  - [ ] 3.3.5 The evaluator onto typed nodes through a read-only body view, with an environment and a session; outcomes keyed by evaluation identity, never by body identity; text provenance carried as `Provenance` segments through slice, concat and call composition, with every selecting call site reporting a shared result at its own argument; construction cycles, recursion and non-terminating evaluation told apart; completed rejections recorded and aborted evaluations not; outcomes record their cost, cache hits are charged it, and nested budget exhaustion is never recorded under the nested identity; residualization and module selection request `Specialize` artifacts. Construction hands it per-expression lowered nodes until 3.3.6.
  - [ ] 3.3.6 Direct construction through a private body builder; delete `FunctionFact`, `StatementFact`, `ExpressionFact`, `TirLowering` and the fact half of `ResidualBody`.
  - [ ] 3.3.7 Codec round-trip and fingerprint fixtures for every body category, specification sync, and an inventory check that nothing imports a deleted schema.
- [x] 3.4 Preserve source-free semantic/static fixtures with current presentation remapping for diagnostics and navigation.

## 4. A4 — JUL-208 sealed preparation (separate work package)

- [x] 4.1 Extend preparation/selection specification deltas and implement intent-specific manifests, normalized configuration and shared selected source-outcome discovery.
- [x] 4.2 Close executable component demand to a monotone fixed point and seal bundles; preserve analysis-only work bounds, partial queries, required-root failures and unused-component exclusion.
- [x] 4.3 Migrate public/compiler/tooling entry points and separate helper requests, removing downstream source reopening; provide resolver-free downstream evidence with a counting resolver.
