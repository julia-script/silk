# Parallel triage procedure

Read this reference only for `silk-triage` runs.

## Required subagent workflow

Triage MUST use subagents. The coordinator owns batch selection, issue grouping, final verdicts,
queue order, and every Linear write. Subagents read and return evidence; they never mutate Linear or
repository files.

If collaboration tools are unavailable or no subagent can be started, report triage as incomplete
and stop before changing issues. Solo triage does not satisfy this skill.

Use up to three parallel investigators in waves. Give each investigator one issue at a time when
possible, including its full description, links, baseline, suspected overlaps, and relevant repo
paths. For larger batches, reuse completed agents with follow-up assignments rather than making one
agent shallowly process many unrelated issues.

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
  evidence
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
recommended_verdict: todo | duplicate | canceled | needs-more-investigation
```

Zero challenges is valid. Do not manufacture objections.

## Fan-in

Resolve disagreements against primary evidence. Uncertainty is not success: leave the issue in
Backlog with a focused investigation comment when the available evidence cannot support Todo,
Duplicate, or Canceled. Triage may organize multiple leads into one survivor, split a broad lead,
or discard weak leads. Preserve useful evidence and native Linear relations.

Unless Julia states a different current priority, rank validated stabilization issues above
features. Within stabilization, correctness and safety come first, then broken documentation or
public contracts, then simplification, dead code, and other debt. Concrete urgency can override
this ordering, but novelty alone cannot.

Write sequentially only after the batch verdicts and relative priority are coherent. Read every
changed issue back before reporting.
