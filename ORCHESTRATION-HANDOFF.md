# Compiler Deep-Review Orchestration — Paused Session Handoff

Paused: 2026-08-21 · Integration branch: **claude/compiler-review-orchestration-p2chbv**
(reset onto `julia/compiler-review-proposals` @ 07ab9bb, base main @ d54c995)

This continues the "Compiler Deep-Review — OpenSpec Orchestration Handoff" plan (see the
original handoff in this repo's history / openspec changes). Twelve OpenSpec changes; waves
A → B → C per the dependency graph in that document. This file records the exact state at pause.

## How progress is preserved

The session git proxy only allows pushing the integration branch, so the three worker branches
are carried as a **git bundle** committed on this branch:

    .handoff/agent-branches.bundle

Restore them with:

    git bundle unbundle .handoff/agent-branches.bundle
    # or: git fetch .handoff/agent-branches.bundle 'refs/heads/agent/*:refs/heads/agent/*'

The bundle contains (basis 07ab9bb):

- `agent/remove-compiler-dead-code` @ f0bca49
- `agent/extract-compiler-shared-helpers` @ 0e5e0da
- `agent/split-compiler-elaboration` @ 9612897

After restoring the branches, delete `.handoff/` from the integration branch before final
delivery (it is session plumbing, not project content).

## State per change (12 total)

### Wave A (both implementation-complete, pending one uncontended `pnpm check`)

1. **remove-compiler-dead-code** — branch `agent/remove-compiler-dead-code` (f0bca49).
   All tasks 1.1–5.2 done; typecheck clean (24/24), biome clean. Full `pnpm check` reached
   2017/2018 compiler tests; the one failure is a 60s vitest timeout in
   `StoredEffectEngineParity.test.ts` under CPU contention from sibling workers — passes in
   isolation (5/5). No golden/diagnostic deltas.
   **Deviation to review:** proposal listed nine zero-caller Analysis exports; only five were
   truly zero-caller. `structLiteralsOf`, `unionLayoutsOf`, `hirUnionConversionsOf`,
   `mirUnionConversionsOf` have live callers in `packages/inspector/src/Registry.ts` and were
   kept. Consider openspec-update-change to record this.
   Remaining: uncontended green `pnpm check`, then archive.

2. **extract-compiler-shared-helpers** — branch `agent/extract-compiler-shared-helpers` (0e5e0da).
   Tasks 1.1–3.3 + 4.1 (typecheck 22/22) + 4.2 (biome) done. `pnpm test`: 2008/2009; same
   load-induced `StoredEffectEngineParity` timeout, passes in isolation. `pnpm check` not run.
   No golden/diagnostic deltas.
   **Deviation to review:** the fifth Presentation requirement-renderer copy
   (`rowExpression`'s `RequirementMemberExpression`, Presentation.ts:65) intentionally left
   inline — it renders a source-spelled `RequirementRoleFact`, not a canonical
   `RequirementRow.Role`; coercing it through the canonical renderer could change output.
   Documented in the wip commit body (0e5e0da).
   Remaining: full `pnpm test` + `pnpm check`, then merge + archive.

### Started early (Wave C)

5. **split-compiler-elaboration** — branch `agent/split-compiler-elaboration` (9612897).
   Tasks 1.1–3.3 all done and checked off. The split: `ExpressionAnalysis.ts`,
   `CallResolution.ts`, `StatementAnalysis.ts`, `HirLowering.ts` extracted;
   `Elaboration.ts` reduced to fact vocabulary + `Result` + visitors + `elaborateModule`
   façade. Actors import Elaboration type-only (no runtime cycles); new actors deliberately
   kept out of the barrel/package exports. In-package callers updated (`Ownership.ts`,
   `OpaqueRealization.ts`).
   Verification: typecheck clean (24/24), biome clean. Compiler suite post-split 2017/2019 —
   both failures were contention timeouts (`DriverNativeAcceptance`, `LexerPressure`), never
   assertions; all golden/determinism/HIR/MIR suites passed, zero intentional deltas.
   Remaining: rerun the two timed-out test files uncontended, full `pnpm check`, and
   `pnpm release:candidate` (package contents changed), then merge + archive.
   Note: this change has no Wave A dependency, but it will merge after Wave A — expect no
   conflicts (Elaboration files are disjoint from Wave A except `Ownership.ts`/
   `OpaqueRealization.ts` caller updates, which may need a trivial resolution).

### Not started

- Wave B: **reconcile-compiler-duplicated-semantics** (blocks on 1+2),
  **harden-compiler-native-boundary** (blocks on 1+2). Dispatch after Wave A merges.
- Wave C: split-compiler-frontend (blocks on 2), split-compiler-type-system (2+3+4),
  split-compiler-mid-end (3), split-compiler-layout (1+2), split-compiler-ir-evaluation (1+4),
  split-compiler-declaration-index (1+2), split-compiler-backends (2+4).

## Known merge conflict (next concrete step)

Merging `agent/remove-compiler-dead-code` then `agent/extract-compiler-shared-helpers` into the
integration tip conflicts in:

- `packages/compiler/src/ModuleSummary.ts` (content conflict — resolve by hand; dead-code and
  helper-extraction both touched it)
- `packages/compiler/src/ToolchainIntegrity.generated.ts` (generated digest — do NOT hand-merge;
  regenerate with the repo's tooling after resolving the rest)

Suggested order: merge dead-code first (clean), then shared-helpers, resolve ModuleSummary,
regenerate ToolchainIntegrity, run `pnpm check` (uncontended — the StoredEffectEngineParity
timeout above was pure CPU contention), then proceed with Wave B.

## Ground rules (unchanged)

Same as the original handoff: green-field, no shims; verification order
`pnpm typecheck` → `pnpm exec biome check .` → `pnpm test` → `pnpm check`;
do not touch `packages/llvm`; Effect conventions; no non-null assertions/as-casts/lint
suppressions; cheap tests (Analysis.evaluate, corpus programs, diagnostic codes+spans only);
revise proposals only via openspec-update-change; archive each finished change with
openspec archive to promote spec deltas.

## Worker prompt template

Reuse the template from the original handoff (section 6): load openspec-apply-change, implement
exactly one change, verify in order, report branch + check output + intentional deltas +
unfinished tasks. Workers commit on `agent/<change-name>`; only the integration branch is
pushable from a remote session.
