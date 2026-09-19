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
  - [ ] 3.3.1 Locate body diagnostics and text origins by anchor, publish spans at one point per revision, and rebuild span-derived identities from anchors.
  - [ ] 3.3.2 Reference declarations, fields and members by identity in TIR and cached products; reuse by key returns the cached object; delete `SemanticRebinding`.
  - [ ] 3.3.3 Give TIR owner-local node and local ids, authored origins, a field registry and the indexed result tables; publish static bodies; make `Tir.encode` canonical and source-free.
  - [ ] 3.3.4 Move static evaluation, residualization, module selection, lifetime flow, control flow, ownership and opaque realization onto TIR and its tables.
  - [ ] 3.3.5 Move occurrences, completion, type hints, inspector, module tooling and the LSP/docgen callers onto TIR and its tables.
  - [ ] 3.3.6 Emit TIR directly from analysis through one body builder; delete `FunctionFact`, `StatementFact`, `ExpressionFact`, `TirLowering` and the fact half of `ResidualBody`.
  - [ ] 3.3.7 Add `CheckedBody` round-trip and canonical-encoding fixtures, sync specifications, and verify no module imports a deleted schema.
- [x] 3.4 Preserve source-free semantic/static fixtures with current presentation remapping for diagnostics and navigation.

## 4. A4 — JUL-208 sealed preparation (separate work package)

- [x] 4.1 Extend preparation/selection specification deltas and implement intent-specific manifests, normalized configuration and shared selected source-outcome discovery.
- [x] 4.2 Close executable component demand to a monotone fixed point and seal bundles; preserve analysis-only work bounds, partial queries, required-root failures and unused-component exclusion.
- [x] 4.3 Migrate public/compiler/tooling entry points and separate helper requests, removing downstream source reopening; provide resolver-free downstream evidence with a counting resolver.
