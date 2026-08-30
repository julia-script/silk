# Linear configuration

Linear is the only ticket store for the `silk-*` workflow.

## Workspace

- Team name: `Juliaortiz`
- Team key: `JUL`
- Team id: `3ad57020-24ab-4422-bb1a-f33314882e2f`
- Canonical project id: `077633f0-eb65-4d72-a034-a70615821203`
- Current project name: `Silk`
- Repository: `julia-script/silk`

Resolve the project by its canonical ID. The display name is user-editable and MUST NOT be used as
identity. Never create a replacement project because the name changed, the project lookup failed,
or a similarly named project exists. If the canonical ID is inaccessible, trashed, or belongs to a
different team, stop and report the exact mismatch.

Never create, rename, archive, cancel, or delete a Linear project as part of these workflows. Project
administration is outside issue discovery, demand, triage, work, and synchronization.

## Status mapping

| Linear status | Workflow meaning |
| --- | --- |
| Backlog | Captured but not triaged. |
| Todo | Triaged and ready, unless it carries the `Blocked` label. |
| In Progress | Claimed by an active implementation task. |
| In Review | Implementation is ready for human or PR review. |
| Done | The accepted change is merged or otherwise delivered. |
| Duplicate | Another linked issue owns the work. |
| Canceled | A triage claim failed or the work was intentionally declined. |

Use Linear's native `duplicateOf`, `blockedBy`, `blocks`, and `relatedTo` relations when they fit.
Create a `Blocked` issue label only when the first real blocked issue needs it; do not create a
large project-specific label taxonomy. Use the existing `Bug`, `Improvement`, and `Feature` labels
as broad Linear labels, while the description carries the precise maintenance theme.

## Selection

Restrict every listing, deduplication pass, and count to canonical project ID
`077633f0-eb65-4d72-a034-a70615821203` unless the
task explicitly needs a workspace-wide duplicate search. For the next work item, select Todo issues
without `Blocked`, ordered by priority and then oldest creation time. An explicitly named issue
overrides automatic selection.

## Mutations

Re-read an issue immediately before changing its state or specification. Preserve fields unrelated
to the current workflow decision. After a write, read the issue back and verify the intended state.

Do not delete issues. Mark confirmed duplicates as Duplicate and link the survivor. Mark failed
triage as Canceled with a short comment naming the failed claim and the evidence that would reopen
it.
