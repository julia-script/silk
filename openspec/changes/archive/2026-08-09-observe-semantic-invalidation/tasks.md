## 1. Canonical Module Surfaces

- [x] 1.1 Add the public `ModuleSurface` actor with immutable module identity, canonical semantic representation, exact equality, and explicit package exports.
- [x] 1.2 Encode every cross-module-observable function, struct, constant, conformance, visibility, duplicate, and unavailable header state without syntax, spans, bodies, diagnostic identities, or object identity.
- [x] 1.3 Add focused surface tests for independently allocated equality, body/span stability, each public-contract mutation family, malformed headers, collision-safe equality, and fresh-process determinism.

## 2. Dependency-Aware Invalidation

- [x] 2.1 Add the public `SemanticInvalidation` actor with canonical per-module observations, ordered reasons, totals, and compiler semantic-environment input.
- [x] 2.2 Implement previous/current dependency-input comparison, union-graph strongly connected components, reverse dependent edges, and surface-stabilizing worklist propagation.
- [x] 2.3 Add fixtures for unrelated edits, body-only dependencies, signature and visibility changes, struct changes, stable and changing dependency chains, fresh/removed modules, changed import outcomes, and malformed recovery.
- [x] 2.4 Add cyclic fixtures covering local edits, stable exposed surfaces, propagated surface changes, and dependency-graph component merges and splits.
- [x] 2.5 Prove observation order, reason counts, and totals are deterministic under root/map reordering and across a fresh process.

## 3. Frontend and Project Integration

- [x] 3.1 Compute module surfaces once in the shared frontend pipeline, retain them in frontend facts, and report the measured surface phase for single- and multi-root analysis.
- [x] 3.2 Extend phase observations with typed deterministic counters needed for semantic invalidation without changing existing timing behavior.
- [x] 3.3 Integrate fresh and adjacent-revision invalidation plans into `ProjectAnalysis`, share surfaces/plans/final reports across root views, and retain the committed-predecessor contract.
- [x] 3.4 Add project tests that assert exact observations and shared view references while proving all semantic and tooling tables are still freshly rebuilt.
- [x] 3.5 Extend LSP session/workspace coverage to prove stale revisions cannot seed later semantic invalidation and protocol behavior remains unchanged.

## 4. Verification and Finalization

- [x] 4.1 Run compiler and LSP focused tests, then `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test`; fix all change-caused failures.
- [x] 4.2 Run `pnpm check`, strict OpenSpec validation, and `pnpm release:candidate` because public compiler exports change.
- [x] 4.3 Update task evidence, sync delta specs, archive the completed change, and merge its verified branch to `main` without staging unrelated codebase-memory artifacts.
