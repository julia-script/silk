---
name: silk-sync
description: Reconcile Silk Linear issues with repository, OpenSpec, branch, and GitHub PR reality, applying only unambiguous status corrections and reporting uncertain drift. Use only when Julia explicitly invokes this skill or asks to sync the Silk queue.
---

# Sync the Silk queue

Read `../../silk-manager/WORKFLOW.md` and `../../silk-manager/LINEAR.md` completely before acting.

This skill performs periodic reconciliation. It may update existing Linear issues. It must not edit
repository source, switch branches, delete worktrees, clean files, push, merge, close PRs, or create
new maintenance issues.

## Procedure

1. Fetch the canonical Linear project by ID from `LINEAR.md` and verify its team. Never resolve or
   recreate it by display name. Read its nonterminal issues. If it cannot be resolved, stop before
   writing.
2. Inspect the current checkout, local and remote branch information already available, active and
   archived OpenSpec changes, and GitHub PRs for `julia-script/silk`. Do not fetch unless Julia asks.
3. Match by explicit Linear link, Linear-generated branch identifier, or an unambiguous issue
   identifier in a branch or PR. Never match by title alone when more than one issue is plausible.
4. Apply high-confidence corrections directly:

   - merged PR or otherwise proven delivery -> Done;
   - open PR with matching work -> In Review;
   - closed unmerged PR with unfinished work -> Todo, with a comment;
   - issue proven owned by another issue -> Duplicate with a native relation;
   - resolved `## Gate` -> remove `Blocked` and keep or return to Todo;
   - Backlog or Todo claim already implemented on the recorded baseline -> Done when delivery is
     proven, otherwise Canceled with evidence.

5. Re-read every issue before and after mutation. Preserve priority, estimate, relations, and
   specification unless the observed transition requires a focused note.
6. Report but do not automatically reset ambiguous stale In Progress work, In Review issues without
   a PR, possible overlaps, unresolved gates, dirty worktrees, or branches that may belong to a live
   agent task. Give the evidence and the smallest next decision.
7. Report applied transitions, unresolved drift, queue head, and coverage. A fully consistent sync
   is a useful result; do not invent cleanup.

Obvious reconciliation does not need a separate proposal/apply ceremony because invocation of this
skill is the scoped authorization. Ambiguous or destructive action still requires Julia's decision.
