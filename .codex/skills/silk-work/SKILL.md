---
name: silk-work
description: Claim and implement one triaged Silk Linear issue, follow repository and OpenSpec rules, verify the change, and finish with a draft pull request. Use only when Julia explicitly invokes $silk-work, names an issue to implement, or asks to work the next queued Silk issue.
---

# Work a Silk issue

Read `../../silk-manager/WORKFLOW.md` and `../../silk-manager/LINEAR.md` completely before acting.
Then read the repository `AGENTS.md` and every specialized skill required by the issue.

Subagents are authorized for investigation, bounded non-overlapping implementation, and review.
Keep one coordinator responsible for the Linear issue and do not let agents edit overlapping files.

Input is an explicit Linear issue or, when absent, the highest-priority oldest Todo issue in the
canonical project ID from `LINEAR.md` without the `Blocked` label. Fetch and verify that project by
ID; never recreate it from its display name. A named issue overrides automatic selection.

## Admission and claim

Before automatic selection, show existing In Progress issues and In Review issues that currently
need Julia. Do not automatically start a third attention-bearing item. A named issue is an explicit
override of this soft cap.

Re-read the selected issue and verify that its problem is still current. If it is already delivered
or invalid, update Linear with evidence and select the next issue only when selection was automatic.
Otherwise set it to In Progress and read it back before editing.

## Implementation

- Work in the current Codex task checkout. Preserve the current task's branch or create a
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
`pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, then `pnpm check`. Run
`pnpm release:candidate` when package contents or exports change. Do not say work is complete when a
required check was skipped or failed; name the exact gap and whether it predates the change.

Self-review every diff through three lenses: does it solve the acceptance criteria, is it simpler
than the code it replaces, and would the tests fail if the behavior regressed? Use an independent
reviewer subagent when it materially improves confidence, especially for medium, large,
cross-package, or high-risk changes. Independently verify its findings.

## Draft PR and handoff

A `silk-work` run is not finished until the completed change has a draft pull request. After
verification and review:

1. Commit only the intended issue changes on a non-`main` branch and push that branch to `origin`.
2. Create a draft PR targeting `main`. Include the Linear issue link, outcome, acceptance evidence,
   exact checks run, risks or deferred checks, and relevant OpenSpec change.
3. If the branch already has an open PR, reuse it rather than creating a duplicate and ensure it is
   still a draft. Never mark the PR ready for review as part of this skill.
4. Read the PR back and require both its URL and draft state before treating the run as complete.
5. Add the confirmed draft PR link and the same verification evidence to Linear, then move the
   issue to In Review and read it back.

Draft-PR creation is a required delivery step, not an optional action requiring a separate prompt.
If committing, pushing, or creating or confirming the draft PR fails, do not claim completion and
do not move the issue to In Review. Preserve the work and report the exact blocker.

If blocked before handoff, do not hide partial work. Add or update `## Gate`, apply `Blocked`, return
the issue to Todo, and comment with what completed and exactly what unblocks it. If implementation
fails without a real external gate, leave it In Progress and report the failure honestly.
