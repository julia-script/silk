## 1. Contract and syntax

- [x] 1.1 Publish the prescriptive module-selection contract and verify OpenSpec proposal/design/deltas/tasks with strict validation.
- [x] 1.2 Parse nested module conditional groups and selective public imports, preserving recovery in every arm; verify parser and formatter tests.

## 2. Demand and selection

- [x] 2.1 Separate full syntax, condition dependency admission and selected module declarations; verify no resolver call or load for inactive imports.
- [x] 2.2 Complete unconditional configuration before selection and reuse ordinary bounded static evaluation for conditions; verify imported helpers, constants, types, profile parameters, invalid dependencies and availability/default cycles with diagnostic code/span/origin assertions.
- [x] 2.3 Implement forward conditional availability and selective publication with original identity; verify selected collisions, missing/inactive names and re-export chains.

## 3. Semantic integration

- [x] 3.1 Make all frontend/index/surface consumers use selected declarations and expose active profile/inactive ranges; verify inactive declarations produce no Effect/ownership/foreign/backend inventories.
- [x] 3.2 Key selected semantic and body reuse by profile and demanded dependencies; verify profile coexistence, equivalent profiles, changed helper bodies, selected source revisions and unloaded-source isolation in existing project tests.
- [x] 3.3 Migrate affected callers, diagnostics/catalogs, examples and reference, removing superseded unconditional assumptions; verify generated artifacts and absence of obsolete contracts.

## 4. Integration verification and delivery

- [x] 4.1 Run pnpm typecheck, pnpm format:check, pnpm lint, pnpm test, then pnpm check and pnpm release:candidate if package contents/exports change; record exact failures and baseline status.
- [x] 4.2 Commit JUL-121 on its gh stack layer with reviewed scope and validation evidence before adding JUL-123 above it.
