---
name: silk-triage
description: Validate, deduplicate, specify, size, and prioritize Silk maintenance issues from the Linear Backlog. Use only when Julia explicitly invokes this skill or asks to triage Silk issues.
---

# Triage Silk issues

Read `../../silk-manager/WORKFLOW.md`, `../../silk-manager/LINEAR.md`,
`../../silk-manager/REVIEW_BASELINE.md`, and `../../silk-manager/TRIAGE.md` completely before
acting. Follow `TRIAGE.md`'s required investigator and skeptic fan-out.

Input may be explicit Linear issue identifiers or a batch size. Default to the five strongest
Backlog issues, selected by likely impact and evidence rather than identifier order. Unless Julia
says otherwise, choose stabilization leads before feature leads for the default batch.

## Procedure

1. Fetch the canonical Linear project by ID from `LINEAR.md` and verify its team. Never resolve or
   recreate it by display name. Read every Backlog issue, the current Todo queue including feature
   issues, and plausible duplicates in every state. The existing Todo queue is required context for
   enforcing stabilization-first priority. Resolve the exact current triage commit. For each selected
   issue, read its Source and Review baselines and inspect changed relevant paths from its previous
   review commit to the triage commit. If the canonical project is unavailable, stop before writing.
2. Launch the parallel investigation waves from `TRIAGE.md`. Every selected issue must receive a
   subagent investigation. Every proposed Todo, Duplicate, or Canceled verdict must receive an
   independent skeptical pass from a different subagent. The coordinator adjudicates and writes;
   it does not substitute for either pass. Give both passes the previous and current SHAs plus the
   relevant changed-path delta.
3. Consolidate fragments that describe one coherent change. Preserve the best issue, add missing
   evidence there, and mark the others Duplicate with a native relation. Split an overly broad lead
   when independent changes have separate value. Leave uncertain groupings separate.
4. For each selected issue, verify the five triage claims in `WORKFLOW.md`. Use the current
   baseline, codebase graph, relevant docs, tests, package exports, open and archived OpenSpec
   changes, and GitHub PRs. Treat the issue's proposed solution as a hypothesis. Explicitly decide
   whether the issue is still wanted, already delivered, outdated, newly owned elsewhere, or in need
   of revised acceptance because of changes since its Review baseline.
5. Resolve investigator and skeptic disagreements against repository evidence. When evidence is
   still insufficient, leave the issue in Backlog and add a focused comment stating what remains to
   investigate. Do not advance an established Review baseline when the old-to-new range could not
   be evaluated. A legacy unresolved or unknown baseline may advance only through the full
   current-state recovery review in `REVIEW_BASELINE.md`.
6. If a claim fails, move the issue to Canceled and comment with the failed claim, evidence, and a
   concrete reopen condition. If another issue owns the work, use Duplicate instead. Apply the
   terminal decision before updating Review baseline to `Stage: triage`, `Outcome: terminal`.
7. If all claims hold, rewrite the description into the queue-ready issue shape from `WORKFLOW.md`.
   Preserve the discovery rationale when verified or replace it with the stronger rationale from
   investigation. Include a concrete `## Why this matters` section that names the present cost or
   risk, affected workflow or system property, causal chain, and consequence of doing nothing.
   Whenever a short example materially clarifies the verified gap, include paired `## Current
state` and `## Desired behavior` snippets under `WORKFLOW.md`. Verify the current excerpt against
   the exact triage baseline, make the desired example express the accepted observable behavior,
   and replace or remove stale intake snippets.
   Never leave Todo with only an outcome, scope, and acceptance list. Make every acceptance item
   observable. Assign priority, estimate, and a broad existing label. Move it to Todo.
   Then update Review baseline to the exact triage commit with `Stage: triage` and either
   `Outcome: confirmed current` or `Outcome: specification revised`.
8. When completion is genuinely blocked, keep it Todo, add the smallest accurate `## Gate`, and
   apply the `Blocked` label. Use native issue relations for issue-to-issue blockers. Split an
   independently useful unblocked slice only when it can ship on its own.
9. Unless Julia says otherwise, place validated stabilization issues from any of the five
   maintenance themes above feature issues. Encode this in Linear using the priority floor and cap
   from `WORKFLOW.md`. Reconcile existing Todo feature priorities only when needed to preserve this
   default, and report every such change. Do not reprioritize In Progress or In Review work.
10. The coordinator writes Linear sequentially, reads each changed issue back, and reports the
    verdict, priority, estimate, investigator/skeptic coverage, previous-to-current review commit,
    and one-line reason. Also name any untriaged issue that plausibly outranks the new queue head.

Language and standard-library changes must have acceptance that includes the OpenSpec artifacts
required by `AGENTS.md`. Do not preserve compatibility paths merely because the current code has
them; this repository is green-field.
