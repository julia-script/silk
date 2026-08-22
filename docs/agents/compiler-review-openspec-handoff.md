# Compiler Deep-Review — OpenSpec Orchestration Handoff

Handed off: 2026-08-21 · Branch: **julia/compiler-review-proposals** (base **main** @ 739150a)

## 1. What this is

A deep review of **packages/compiler** (twelve parallel subagent review reports, consolidated)
produced twelve remediation changes. All twelve are staged here as planning-complete OpenSpec
changes (proposal + design + tasks, plus spec deltas for the two behavior-changing ones). All
twelve pass **openspec validate**. No compiler source code has been implemented. You are
orchestrating the implementation across multiple worker sessions.

Out of scope: **packages/llvm** (owned by a different agent — do not touch it) and the
uncommitted .claude/ and packages/llvm/** working-tree edits that belong to that other agent. Do
not git-add, commit, or modify those.

## 2. The twelve changes

| # | Change (dir under openspec/changes/) | Kind | Spec delta home | Primary files touched |
| - | --- | --- | --- | --- |
| 1 | remove-compiler-dead-code | refactor (skip_specs) | — | Analysis, Type, Mir, Hir, SuspensionMir, DeclarationIndex, ModuleTooling, SyntaxCorrespondence/ProjectAnalysis, CallableFieldRealization, OpaqueRealization |
| 2 | extract-compiler-shared-helpers | refactor (skip_specs) | — | ModuleClosure, DeclarationIndex, OpaqueRealization, ModuleSummary, ImportPlan, NameResolution, Layout, Backend, WasmBackend, Lexer, LiteralForm, StaticText, FloatingPoint, Transcendental, Type, Presentation, PhaseReport, Pipeline, Driver |
| 3 | reconcile-compiler-duplicated-semantics | behavior | bootstrap-complete-interface-contracts | Type, TypeCompatibility, InterfaceWitnessCompatibility, Ownership, SuspensionOwnership, Lower, Token |
| 4 | harden-compiler-native-boundary | behavior | bootstrap-native-toolchain, bootstrap-backend | NativeToolchain, Driver, Target, Backend, WasmBackend, BootstrapEvaluation |
| 5 | split-compiler-frontend | refactor (skip_specs) | — | Parser, Lexer |
| 6 | split-compiler-type-system | refactor (skip_specs) | — | Type, Instances, Pipeline |
| 7 | split-compiler-elaboration | refactor (skip_specs) | — | Elaboration |
| 8 | split-compiler-mid-end | refactor (skip_specs) | — | Lower, Ownership, SuspensionOwnership |
| 9 | split-compiler-layout | refactor (skip_specs) | — | Layout, OpaqueRealization, CallableFieldRealization, Scalar, Match |
| 10 | split-compiler-ir-evaluation | refactor (skip_specs) | — | Mir, ProvisionalMir, SuspensionMir, BootstrapEvaluation |
| 11 | split-compiler-declaration-index | refactor (skip_specs) | — | DeclarationIndex |
| 12 | split-compiler-backends | refactor (skip_specs) | — | Backend, WasmBackend |

skip_specs: true means the change is a pure refactor with no observable behavior change; its
.openspec.yaml already carries the marker, so openspec validate accepts it with zero spec deltas.

## 3. Dependency graph (waves)

Edges are hard serialization because two changes edit the same file. Everything not listed is
parallelizable.

Wave A — foundation (run first; 1 and 2 in parallel):

- remove-compiler-dead-code — deletes symbols that later changes rename or relocate.
- extract-compiler-shared-helpers — creates the internal helpers every split imports.

Wave B — semantic + boundary (3 and 4 in parallel, both after Wave A):

- reconcile-compiler-duplicated-semantics — after 1 + 2 (edits Type.ts, Ownership, Lower).
- harden-compiler-native-boundary — after 1 + 2 (edits Backend, WasmBackend, BootstrapEvaluation).

Wave C — the eight splits (parallelize, honoring these edges):

| Change | Blocks on |
| --- | --- |
| split-compiler-elaboration | — (only touches Elaboration.ts; can start in Wave B) |
| split-compiler-frontend | 2 (Lexer byte-classification move) |
| split-compiler-type-system | 2 + 3 (Type.ts) and 4 (Pipeline/Driver) |
| split-compiler-mid-end | 3 (Ownership/SuspensionOwnership/Lower) |
| split-compiler-layout | 1 + 2 (OpaqueRealization/CallableFieldRealization/Layout) |
| split-compiler-ir-evaluation | 1 + 4 (Mir, BootstrapEvaluation) |
| split-compiler-declaration-index | 1 + 2 (DeclarationIndex) |
| split-compiler-backends | 2 + 4 (Backend/WasmBackend) |

Recommended schedule: A → B (+ elaboration in parallel any time) → C in two mini-batches of four,
picking membership so no two mini-batch members touch the same file.

## 4. Ground rules (non-negotiable, from AGENTS.md)

- Green-field: no compatibility contract. Implement the clean target design, delete superseded
  code, do not keep shims/aliases/fallbacks. A change is incomplete while an obsolete path remains.
- Verification order for every change: pnpm typecheck → pnpm exec biome check . → pnpm test.
  Run pnpm check as the final gate before handing a change back.
- Do not touch packages/llvm or the other agent's uncommitted working-tree edits.
- Effect conventions: one module per actor, data-first + dual, Effect.fn / Effect.fnUntraced, wrap
  external APIs in Effect (no bare throw or Promise across a boundary), @effect/vitest tests.
- No non-null assertions, no as-casts except truths TypeScript cannot express, no lint suppressions.
- Tests stay cheap: prove semantics with Analysis.evaluate; add corpus programs to
  test/support/corpus.ts rather than per-feature native-parity tests; assert diagnostic codes and
  spans, never message text; no per-feature fresh-process determinism tests.
- If a proposal needs revision during implementation, use the openspec-update-change skill, never
  hand-edit the artifacts to force validation.

## 5. Orchestration protocol

1. For each change you dispatch, create a branch agent/<change-name> from
   julia/compiler-review-proposals (or from the current integration tip).
2. Give the worker exactly one change, the worker prompt below, and the gating rules in section 4.
3. Worker applies via the openspec-apply-change skill (tasks.md drives the work; applyRequires is
   already satisfied since tasks are present).
4. Worker returns: branch name, pnpm check result, a list of every diagnostic-code/span/golden
   change with justification, and any unfinished task.
5. You review the diff scope (the change must not wander outside its listed files), merge the
   branch back into the integration branch, re-run pnpm check once, then dispatch the next wave.
6. On a conflict you cannot resolve silently (two changes both need an overlapping hunk), pull the
   later change into the same session rather than editing two proposals in parallel.
7. After a change is fully implemented and verified, archive it with openspec archive <change> (or
   the openspec-archive-change skill) to promote its spec deltas into openspec/specs/.

## 6. Worker prompt template

Load the openspec-apply-change skill, then implement the OpenSpec change openspec/changes/<NAME>
exactly as specified — proposal/design/tasks are authoritative. Follow AGENTS.md green-field and
Effect conventions. Touch only the files the proposal lists (plus the callers/tests/docs that must
move with them). Verify in order: pnpm typecheck, pnpm exec biome check ., pnpm test, then pnpm
check. Report the final check output, every intentional diagnostic-code/span/golden change, and any
task you could not complete. Do not touch packages/llvm, do not edit the change's own planning
artifacts, and do not modify unrelated working-tree state.

## 7. Done criteria

- All twelve changes applied, merged to one integration branch, and archived (spec deltas synced).
- pnpm check green on the integration branch at the end.
- Each refactor is byte-determinism clean (golden byte comparisons unchanged except where a
  behavior change — items 3 and 4 — intentionally alters them and the delta is documented).