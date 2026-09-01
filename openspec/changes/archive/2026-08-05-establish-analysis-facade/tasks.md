## 1. The facade

- [x] 1.1 Create `packages/compiler/src/Analysis.ts`: snapshot from a compilation request plus
      `ofSource` convenience; queries for modules, syntax, imports and cycles, declaration index and
      lookups, elaborated facts, HIR, and `rootAnalysis`
- [x] 1.2 Merged driver-order diagnostics across every module and phase
- [x] 1.3 `evaluate(snapshot)` query for the root module
- [x] 1.4 Facade tests: multi-module queries, merged diagnostics, recovery-state queryability,
      repeat determinism, evaluation
- [x] 1.5 Export from the package index and exports map; release-candidate surface; document the
      facade-only rule in the package README

## 2. Inspector migration

- [x] 2.1 Syntax lab builds a facade snapshot; facts, HIR, diagnostics, and evaluation read from
      facade queries
- [x] 2.2 Module-closure and declaration-index labs build snapshots and query the facade
- [x] 2.3 Remaining phase-namespace imports in `apps/docs` become type-only
- [x] 2.4 Add the import-boundary test failing on value imports of phase modules from the labs

## 3. Verification

- [x] 3.1 Full compiler and docs suites pass; `pnpm check` and release-candidate green
- [x] 3.2 `openspec validate establish-analysis-facade --type change --strict` passes
