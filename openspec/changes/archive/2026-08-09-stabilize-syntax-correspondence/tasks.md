## 1. Syntax Correspondence Actor

- [x] 1.1 Add deterministic exact structural fingerprints and conservative unique-sibling matching for adjacent syntax files.
- [x] 1.2 Expose bidirectional element lookup, ordered canonical identity pairs, and immutable correspondence counts.
- [x] 1.3 Add tests for shifted/reordered declarations, edited subtrees, ambiguous duplicates, malformed recovery, foreign sources, and fresh-process determinism.
- [x] 1.4 Add the public compiler subpath export, package documentation, and release-candidate coverage.

## 2. Revision-Aware Project Analysis

- [x] 2.1 Let project closure loading reuse a prior syntax artifact only when module identity, source origin, and bytes are equal.
- [x] 2.2 Add `ProjectAnalysis.revise` and per-module fresh/reused/changed syntax observations with correspondence for changed modules.
- [x] 2.3 Preserve whole-project semantic recomputation and add tests proving syntax reference reuse, fresh changed syntax, new/removed module handling, and new semantic fact tables.

## 3. LSP Accepted-Revision Integration

- [x] 3.1 Pass the last committed analyzed-document map into each ProjectSession analysis callback without allowing stale work to seed later revisions.
- [x] 3.2 Retain the shared ProjectAnalysis on workspace document results and revise it from the prior committed workspace result.
- [x] 3.3 Add scheduler and workspace tests for initial empty history, accepted revision reuse, stale revision exclusion, shared current project identity, and unchanged dependency syntax reuse.

## 4. Verification

- [x] 4.1 Run typecheck, Biome, focused and full tests, `pnpm check`, OpenSpec strict validation, and `pnpm release:candidate`.
