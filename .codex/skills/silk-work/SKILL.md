---
name: silk-work
description: Claim and implement one triaged Silk Linear issue, follow repository and OpenSpec rules, verify the change, and finish with a draft pull request. Use only when Julia explicitly invokes this skill, names an issue to implement, or asks to work the next queued Silk issue.
---

# Work a Silk issue

Read `../../silk-manager/WORKFLOW.md`, `../../silk-manager/LINEAR.md`,
`../../silk-manager/REVIEW_BASELINE.md`, and `../../silk-manager/TEST_REVIEW.md` completely before
acting. Then read the repository `AGENTS.md` and every specialized skill required by the issue.

Subagents are authorized for investigation, bounded non-overlapping implementation, and review.
Keep one coordinator responsible for the Linear issue and do not let agents edit overlapping files.

Input is an explicit Linear issue or an automatic queue selection under `LINEAR.md`. Fetch and
verify the canonical project by ID; never recreate it from its display name. For automatic
selection, first read all Todo issues. If Todo is nonempty, choose its highest-priority oldest
unblocked issue; when every Todo issue is blocked, stop rather than falling back. Only when Todo is
literally empty, fall back to the highest-priority oldest unblocked Backlog issue whose description
records `Triage disposition: queue-ready`. A named queue-ready Backlog issue or a Todo issue
overrides automatic selection; never claim an issue still in Triage.

## Admission and claim

Before automatic selection, show existing In Progress issues and In Review issues that currently
need Julia. Do not automatically start a third attention-bearing item. A named issue is an explicit
override of this soft cap.

Record whether the selected issue came from Todo or Backlog before claiming it. This pre-claim tier
controls where interrupted or blocked work returns; agents never promote fallback work into Julia's
Todo queue.

Re-read the selected issue and its Review baseline. Resolve the exact verified work-base commit and
inspect the changed relevant paths from the previous review commit under `REVIEW_BASELINE.md`.
Revalidate whether the work is still wanted, already delivered, outdated, superseded, or incorrectly
specified before claiming it. If it is already delivered or invalid, apply the truthful Linear state
and, for a Canceled or Duplicate result, set `Triage disposition: terminal`; then update Review
baseline at the work-base commit with the corresponding Outcome. Select the next issue only when
selection was automatic. If the delta cannot be reviewed, do not claim or
advance an established baseline. A legacy unresolved or unknown baseline requires the full
current-state recovery review from `REVIEW_BASELINE.md` before claiming. Otherwise update the issue
specification if needed, including refreshing or removing current/desired snippets invalidated by
the reviewed delta and correcting the Area label when primary ownership changed, set Review
baseline to `Stage: work admission` and
`Outcome: implementation ready`, then set it to In Progress and read it back before editing.

## Implementation

- Work in the current agent task checkout. Preserve the current task's branch or create a
  `julia/`-prefixed feature branch when needed for the required draft PR. Do not switch to or commit
  on `main`.
- Preserve unrelated and uncommitted changes. Stop if they overlap in a way that cannot be handled
  safely.
- For any Silk language or standard-library change, use the project OpenSpec workflow before
  implementation. Use OpenSpec for other medium or large architectural changes when a written
  design materially reduces risk.
- Implement the clean green-field design and update all callers, tests, fixtures, generated
  artifacts, and docs in the same change. Remove the superseded path.
- Meet the Linear acceptance criteria literally. If investigation changes the correct solution,
  update the issue specification before implementing the new scope.

## Verification and review

Use the cheapest tests that falsify each claim, while following the repository's required order:
`pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`, then `pnpm check`. Run
`pnpm release:candidate` when package contents or exports change. Do not say work is complete when a
required check was skipped or failed; name the exact gap and whether it predates the change.

Do not add tests by default merely because production code changed. Every new or expanded test must
justify its unique signal, execution complexity, optimization state, and measured contribution to
default-suite time under `TEST_REVIEW.md`.

Self-review every diff through three lenses: does it solve the acceptance criteria, is it simpler
than the code it replaces, and would the tests fail if the behavior regressed? Use an independent
reviewer subagent when it materially improves confidence, especially for medium, large,
cross-package, or high-risk changes. Independently verify its findings.

After focused verification stabilizes, assign a separate mandatory test-economics reviewer
subagent. It must follow `TEST_REVIEW.md`, inspect the exact issue-scoped diff and affected test
execution, measure the runtime cost, and return `approve` before handoff. The coordinator verifies
and fixes valid findings, reruns affected checks, and repeats the review until the final committed
PR diff is approved. A general reviewer cannot double as the test-economics reviewer.

## Draft PR and handoff

A `silk-work` run is not finished until the completed change has a draft pull request. After
verification and review:

1. Commit only the intended issue changes on a non-`main` branch.
2. Give the test-economics reviewer the exact committed issue diff. If commit hooks, generated
   artifacts, or review fixes alter it, rerun affected checks, recommit, and repeat review until the
   committed diff is approved.
3. Push the approved branch to `origin`.
4. Create a clear draft PR targeting `main` and follow the Pull request quality bar in
   `WORKFLOW.md`. Include the Linear issue link, outcome, acceptance evidence, exact checks run,
   test-review verdict and timing delta, risks or deferred checks, and relevant OpenSpec change.
   Strongly prefer concise `Before` and `After` code or output examples when they materially clarify
   the delivered change; omit them only when examples would add no useful signal or would
   misrepresent the work.
5. If the branch already has an open PR, reuse it rather than creating a duplicate and ensure it is
   still a draft. Never mark the PR ready for review as part of this skill.
6. Read the PR back and require both its URL and draft state before treating the run as complete.
7. Add the confirmed draft PR link and the same verification evidence to Linear. Update Review
   baseline to the exact committed PR-head SHA with `Context: PR <number> head`,
   `Stage: implementation handoff`, and `Outcome: implementation complete`; then move the issue to In
   Review and read it back.

Draft-PR creation is a required delivery step, not an optional action requiring a separate prompt.
If committing, pushing, or creating or confirming the draft PR fails, do not claim completion and
do not move the issue to In Review. Preserve the work and report the exact blocker.

If blocked before handoff, do not hide partial work. Add or update `## Gate`, apply `Blocked`, return
the issue to its pre-claim queue tier, and comment with what completed and exactly what unblocks it.
Use Backlog when the prior tier cannot be established; never fill Todo by inference. If
implementation fails without a real external gate, leave it In Progress only while this task
remains the active owner and will continue the same implementation. If the run is ending or
abandoned with no live owner, restore the pre-claim tier, do not apply `Blocked`, and comment with
the completed work and unresolved failure.
