---
name: slp-5-implement
description: "SLP step 5 of 6. Implement every OpenSpec change handed off from one accepted SLP: dependency-ordered, parallel where independent, each change gated by hard checks and a single-pass conformance check with bounded fix attempts. Manual-only: use ONLY when the user explicitly invokes /slp-5-implement or says \"implement this SLP\". Never trigger from generic implement/apply requests."
---

# SLP 5: Implement

Pipeline: 1 develop → 2 review → 3 resolve → 4 handoff → **5 implement** → 6 audit-implementation.

Read `AGENTS.md`, `proposals/PROCESS.md` (§ Traceability gates), the target SLP, and every change in
its `OpenSpec handoff` field before acting. Require every change to have a `Ready` handoff audit;
otherwise stop and point to `/slp-4-handoff --audit-only`. This skill owns all iteration; no
sub-agent may loop on its own.

## Build the plan

1. Read each change's proposal for `Depends on` and its tasks. Build the dependency DAG; changes
   with no unmet dependency form a layer.
2. Pick the integration branch (current branch unless the user names one). Each change gets its own
   git worktree off the integration branch.
3. Publish a visible plan: layers, changes per layer, and per-change state
   (`pending | running | done | parked`). Keep it current.

Cap parallelism at the layer width; compiler changes touch shared files, so wider is not faster.

## Per change (in parallel within a layer)

Run each change in a sub-agent in its worktree with exactly this contract:

1. **Apply.** Follow `/openspec-apply-change` mechanics: `openspec status`, `openspec instructions
   apply --json`, read all `contextFiles`, implement tasks in order, check boxes as completed.
2. **Hard gates.** Run the repository's required checks (format, typecheck, full test gate).
   On failure: at most **3** fix attempts. Each attempt must target a different root cause than the
   last; if the same failure recurs after a fix, re-localize (re-read the failing path end to end)
   instead of retrying. Three failures → return `parked: gates` with the exact failing output.
3. **Conformance check (single pass).** Spawn three lens agents (language behavior, OpenSpec
   conformance, architecture/privilege — as in step 6) with the diff, tests, SLP, and change
   artifacts. Findings only, structured with `claim | severity | evidence`. Verify each finding
   yourself. Fix only verified in-scope Critical/High, **one** fix pass, rerun hard gates once.
   Remaining blockers → `parked: conformance` with the findings. Findings that need an SLP or
   OpenSpec decision → `parked: decision`; do not improvise the answer.
4. **Report.** Return `done` or `parked: <reason>`, attempts used, commit range, and the conformance
   findings ledger. Write nothing outside the worktree.

## Layer barrier

After a layer finishes: merge each `done` worktree into the integration branch in DAG order, then
run the full hard gate set once on the integrated result. A regression here gets **one** fix attempt
on the integration branch; otherwise park the offending change and revert its merge.

A `parked` change parks every downstream change. Independent siblings continue. Never auto-accept on
cap exhaustion — compiler work fails closed.

## Finish

Remove worktrees. Report one table: change, layer, state, attempts, stop reason, findings count.
List parked changes with the exact next action (`/slp-4-handoff --audit-only`, `/slp-3-resolve`,
or a named gate failure). Record the plan and per-change outcomes in
`proposals/NNNN-slug/audits/implementation-<slp>-plan.md`.

Next: `/slp-6-audit-implementation` across all changes, then archive.
