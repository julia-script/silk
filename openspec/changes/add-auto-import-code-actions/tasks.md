## 1. Compiler Inventory

- [ ] 1.1 Add compiler tests and the `ModuleSummary` actor for compact import/public-export extraction from available declaration headers, including recovered and duplicate-name exclusion.
- [ ] 1.2 Add compiler tests and the immutable `WorkspaceInventory` actor with separate project/toolchain tiers, exact by-name lookup, deterministic ordering, per-module replacement/removal, and unchanged-summary identity reuse.
- [ ] 1.3 Export the new compiler actors through explicit package subpaths and the package's public namespace barrel.

## 2. Source Actions and Import Planning

- [ ] 2.1 Add compiler tests and the protocol-neutral `SourceAction` descriptor/change-plan actor, including source preconditions and validation of grouped non-overlapping edits.
- [ ] 2.2 Add focused `ImportPlan` tests for new imports and inline, multiline, hybrid, namespace-aliased, member-aliased, duplicate, and damaged existing imports.
- [ ] 2.3 Implement `ImportPlan` as surgical syntax-owned edits that preserve existing aliases/trivia and withhold plans that cannot be applied coherently.
- [ ] 2.4 Add compiler tests and `AutoImport` discovery for unresolved occurrence selection, namespace/kind applicability, visibility, collision filtering, existing-import preference, project/toolchain ranking, and ambiguous-module actions.
- [ ] 2.5 Expose auto-import discovery and resolution through the compiler `Analysis` facade while keeping inventory inputs explicit and LSP types outside the compiler.

## 3. Workspace Inventory Lifecycle

- [ ] 3.1 Add LSP tests and the Effectful `WorkspaceCatalog` actor for sorted canonical source-root enumeration, open-buffer precedence, symlink/outside-root rejection, and toolchain-manifest summary construction.
- [ ] 3.2 Replace project-session's bare invalidation priority with structured document, dirty-path, and rediscovery invalidations while preserving debounce, latest-wins scheduling, and exact-version acquisition.
- [ ] 3.3 Build or revise the workspace inventory during project analysis and commit the same immutable inventory with every analyzed-document view and shared `ProjectAnalysis` revision.
- [ ] 3.4 Route watched create/change/delete events to exact dirty modules, route manifest/source-root changes to rediscovery, and remove deleted summaries without rescanning unrelated modules.
- [ ] 3.5 Add lifecycle tests for initial discovery, unsaved exports, closed-file create/change/delete/rename, toolchain candidates, stale revisions, and proof that inventory candidates do not become semantic project roots.

## 4. LSP Code-Action Delivery

- [ ] 4.1 Adapt existing diagnostic edits through shared source-action-to-LSP conversion while preserving titles, diagnostic association, ordering, range filtering, and negotiated position encoding.
- [ ] 4.2 Advertise code-action resolve support and return serializable auto-import descriptors containing the originating URI/version, target, candidate module, spelling, and kind.
- [ ] 4.3 Resolve descriptors by reacquiring the originating revision, repeating candidate applicability, constructing the import plan, and returning a disabled/no-edit action for stale or no-longer-applicable candidates.
- [ ] 4.4 Add protocol tests for one candidate, ambiguous candidates, wrong-kind/private/colliding exclusions, existing-import extension, new-import insertion, range filtering, UTF-16 ranges, and stale resolution.

## 5. Scale and Verification

- [ ] 5.1 Add deterministic inventory observations for scanned, reused, revised, removed, and indexed module/export counts plus elapsed discovery and summary time.
- [ ] 5.2 Add a generated large-workspace regression proving exact lookup avoids semantic analysis of unrelated modules and a one-module edit reuses every unaffected summary.
- [ ] 5.3 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test`, fixing every change-caused failure.
- [ ] 5.4 Run `pnpm check` and `pnpm release:candidate`, and record initial scan, incremental revision, and auto-import query measurements in the change handoff.
