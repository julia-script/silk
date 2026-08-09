## 1. Module Semantic Ownership

- [x] 1.1 Add the public immutable `ModuleSemantics` actor owning one module's elaboration and ownership facts, with explicit compiler exports and focused construction tests.
- [x] 1.2 Remove the closure-wide declaration index from `Elaboration.Result`, pass the current index explicitly to ownership analysis, and update all compiler callers and tests without compatibility shims or casts.
- [x] 1.3 Canonicalize source-backed semantic occurrence locations against the current declaration index, including declarations and fields captured from semantically equal predecessor dependencies.

## 2. Incremental Project Frontend

- [x] 2.1 Move project semantic invalidation into the project pipeline after current surfaces and before elaboration, accepting an optional prior immutable reuse basis.
- [x] 2.2 Build the current module-artifact map by structurally sharing only reusable same-module prior artifacts and recomputing every conservative fallback or invalidated module.
- [x] 2.3 Derive the existing elaboration and ownership maps from module artifacts, keep global current declaration/resolution/diagnostic/tooling construction, and preserve one complete immutable publication boundary.
- [x] 2.4 Add typed elaboration/ownership reuse counters and prove their deterministic phase values without timing thresholds.

## 3. Facade and LSP Contracts

- [x] 3.1 Introduce distinct single-root and project-view realization discriminators so frontend queries accept both while runtime realization accepts only single-root snapshots.
- [x] 3.2 Update ProjectAnalysis and LSP document/workspace/session types to consume project views without weakening atomic committed-predecessor scheduling.
- [x] 3.3 Add compiler and LSP type/runtime coverage proving project views stay query-compatible, cannot be realized, and stale revisions cannot seed shared module artifacts.

## 4. Reuse Correctness

- [x] 4.1 Add focused fixtures for fresh analysis, unrelated edits, body-only dependency edits, signature/visibility/struct changes, dependency chains, cycles, missing prior artifacts, and malformed recovery.
- [x] 4.2 Assert exact shared/non-shared artifact identities, elaboration and ownership execution counters, current navigation spans, diagnostics, root-order determinism, and fresh-process determinism.

## 5. Verification and Finalization

- [x] 5.1 Run focused compiler and LSP suites, `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test`; fix every change-caused failure.
- [x] 5.2 Run `pnpm check`, strict OpenSpec validation, and `pnpm release:candidate` because compiler exports and public types change.
- [x] 5.3 Update task evidence, sync delta specs, archive the completed change, and merge its verified branch to `main` without staging unrelated codebase-memory or architecture-guide artifacts.
