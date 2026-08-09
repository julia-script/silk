## 1. Multi-root Closure and Pipeline

- [x] 1.1 Generalize ModuleClosure loading to accept deterministic canonical root sets while preserving the single-root entry point.
- [x] 1.2 Add union-closure tests for shared dependencies, root-order determinism, conflicting roots, diagnostics, and one parse per module.
- [x] 1.3 Add project frontend orchestration that executes every frontend compiler phase once over the union closure.

## 2. Project Analysis Actor

- [x] 2.1 Add the public ProjectAnalysis actor with immutable project data and root-view lookup.
- [x] 2.2 Build semantic-occurrence and anonymous-expression tooling indexes once per project revision and share phase observations across views.
- [x] 2.3 Add ProjectAnalysis tests for root identity, shared references, module-qualified isolation, phase order, and frontend-only behavior.
- [x] 2.4 Add explicit package exports, release-candidate coverage, and public documentation for project analysis.

## 3. Project Session Integration

- [x] 3.1 Change ProjectSession analysis to one captured-revision callback returning the complete analyzed-document map.
- [x] 3.2 Preserve latest-wins scheduling, atomic commit, exact-version acquisition, publication, shutdown, and independent-project concurrency tests.
- [x] 3.3 Add coverage proving the analysis callback executes once for a multi-document accepted revision.

## 4. Workspace and Server Integration

- [x] 4.1 Add Workspace project analysis using all synchronized roots and one overlay resolver.
- [x] 4.2 Compute shared module URI mappings once and derive every analyzed document from the same ProjectAnalysis value.
- [x] 4.3 Migrate Server wiring and add LSP integration coverage for overlapping dependencies, unrelated roots, and frontend-only shared views.

## 5. Verification

- [x] 5.1 Run typecheck, Biome, tests, `pnpm check`, OpenSpec strict validation, and `pnpm release:candidate`.
