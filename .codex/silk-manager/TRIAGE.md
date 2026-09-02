# Parallel triage procedure

Read this reference only for `silk-triage` runs.

## Required subagent workflow

Triage MUST use subagents. The coordinator owns batch selection, issue grouping, final verdicts,
queue order, and every Linear write. Subagents read and return evidence; they never mutate Linear or
repository files.

If collaboration tools are unavailable or no subagent can be started, report triage as incomplete
and stop before changing issues. Solo triage does not satisfy this skill.

Use up to three parallel investigators in waves. Give each investigator one issue at a time when
possible. Before assignment, read its Source and Review baselines, resolve the exact triage commit,
and inspect the old-to-new changed relevant paths under `REVIEW_BASELINE.md`. Include the full
description, links, previous review commit, current triage commit, changed paths, suspected overlaps,
and relevant repo paths. For larger batches, reuse completed agents with follow-up assignments
rather than making one agent shallowly process many unrelated issues.

Each issue receives two independent passes:

1. **Investigation** — establish the real current behavior, ownership, scope, likely change, and
   evidence for every triage claim.
2. **Skeptical review** — a different agent tries to refute the proposed verdict, find an existing
   owner or simpler framing, and challenge the proposed grouping and acceptance.

Every selected issue must receive its investigation from a subagent. Every proposed Todo,
Duplicate, or Canceled verdict must then receive skeptical review from a different subagent. The
coordinator adjudicates the two passes against primary evidence but does not substitute for either
one. For an unresolved issue that remains Backlog, the coordinator may stop after investigation
only when the investigator identifies the exact missing evidence and no verdict is proposed.

## Investigator return

```text
issue: JUL-N
review_delta:
  previous_review_commit
  current_review_commit
  changed_relevant_paths
  effect_on_currentness_and_scope
verdicts:
  real: {result: true|false|uncertain, evidence}
  current: {result: true|false|uncertain, evidence}
  in_scope: {result: true|false|uncertain, evidence}
  worthwhile: {result: true|false|uncertain, evidence}
  bounded: {result: true|false|uncertain, evidence}
overlaps:
  - issue, PR, or OpenSpec change and relationship
recommended_shape:
  title
  pitch
  why_it_matters:
    current_cost_or_risk
    affected_workflow_or_property
    causal_chain
    consequence_if_ignored
  evidence
  current_state_snippet_if_useful:
    path_and_symbol
    exact_excerpt
    observed_behavior
  desired_behavior_snippet_if_useful:
    concrete_example
    intended_observable_difference
  acceptance
  gate
  priority
  estimate
decision: todo | duplicate | canceled | needs-more-investigation
out_of_scope:
  - concrete observation deliberately excluded
```

## Skeptic return

```text
issue: JUL-N
challenges:
  - claim challenged, counter-evidence, and consequence
grouping_review:
  - merge, split, keep separate, or no concern
acceptance_gaps:
  - condition that is vague, solution-prescriptive, or cannot falsify completion
snippet_gaps:
  - missing, stale, inaccurate, oversized, or unnecessarily implementation-prescriptive example
justification_gaps:
  - missing evidence, broken causal link, generic rationale, or consequence not established
recommended_verdict: todo | duplicate | canceled | needs-more-investigation
```

Zero challenges is valid. Do not manufacture objections.

## Fan-in

Resolve disagreements against primary evidence. Uncertainty is not success: leave the issue in
Backlog with a focused investigation comment when the available evidence cannot support Todo,
Duplicate, or Canceled. Triage may organize multiple leads into one survivor, split a broad lead,
or discard weak leads. Preserve useful evidence and native Linear relations.

The coordinator must preserve the lead's `Why this matters` rationale when it survives review or
replace it with the more precise rationale supported by investigation. Do not promote a
solution-only issue. A Todo description must make the causal chain from current condition to cost
or risk understandable without reading discovery comments, subagent reports, or conversation
history. When paired current and desired snippets would clarify the verified gap, the coordinator
must include them under `WORKFLOW.md`: the current excerpt is exact at the triage baseline and the
desired example expresses the accepted observable outcome without locking in an unnecessary
implementation. Replace provisional, inaccurate, or stale intake snippets rather than preserving
them as historical text. If the condition is real but no concrete consequence can be established,
it is not yet worthwhile: leave it in Backlog for named evidence or cancel it when further
investigation is not justified.

Unless Julia states a different current priority, rank validated stabilization issues above
features. Within stabilization, correctness and safety come first, then broken documentation or
public contracts, then simplification, dead code, and other debt. Concrete urgency can override
this ordering, but novelty alone cannot.

Write sequentially only after the batch verdicts and relative priority are coherent. Read every
changed issue back before reporting. Every Todo, Duplicate, Canceled, or deliberately retained
Backlog issue that completed technical review receives `Stage: triage` and the appropriate Outcome
in Review baseline. Apply the verdict or revised specification first; advance the baseline only
after the issue reflects the conclusion. If the commit range could not be reviewed, retain the old
baseline and leave the issue in Backlog with the exact missing evidence, except for the explicit
from-scratch legacy recovery in `REVIEW_BASELINE.md`.
