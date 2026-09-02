# Linear configuration

Linear is the only ticket store for the `silk-*` workflow.

## Workspace

- Team name: `Juliaortiz`
- Team key: `JUL`
- Team id: `3ad57020-24ab-4422-bb1a-f33314882e2f`
- Native Triage status id: `e46e2b85-b03f-4504-b1a6-c1ae410b4a1b`
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

| Linear status | Workflow meaning                                              |
| ------------- | ------------------------------------------------------------- |
| Triage        | Captured intake that has not passed technical triage.         |
| Backlog       | Triaged, prioritized fallback queue for agent-selected work.  |
| Todo          | Julia's manually curated immediate queue of triaged work.     |
| In Progress   | Claimed by an active implementation task.                     |
| In Review     | Implementation is ready for human or PR review.               |
| Done          | The accepted change is merged or otherwise delivered.         |
| Duplicate     | Another linked issue owns the work.                           |
| Canceled      | A triage claim failed or the work was intentionally declined. |

Use Linear's native `duplicateOf`, `blockedBy`, `blocks`, and `relatedTo` relations when they fit.
Create a `Blocked` issue label only when the first real blocked issue needs it; do not create a
large project-specific label taxonomy. Use the existing `Bug`, `Improvement`, and `Feature` labels
as broad Linear labels, while the description carries the precise maintenance theme.

## Area labels

The team has an optional, mutually exclusive `Area` issue-label group:

| Label              | ID                                     | Apply when the primary owned surface is                                           |
| ------------------ | -------------------------------------- | --------------------------------------------------------------------------------- |
| `Docs`             | `c474a43d-ee5c-45a1-89f6-879b5f4de89e` | reference, tutorials, READMEs, examples, generated docs, or documentation tooling |
| `Standard library` | `97c9816f-e6ad-4e41-bc26-9f54984b5fee` | standard-library APIs, implementations, runtime contracts, or generated surfaces  |
| `Compiler`         | `f1ae3797-6ffb-4e1d-9801-11913fa5695b` | frontend, analysis, IR, ownership, evaluation, diagnostics, lowering, or backends |
| `LSP`              | `2e78d926-3248-4680-8031-092813c98825` | language-server behavior, protocol integration, or editor language intelligence   |

Apply exactly one Area label only when one of these is the issue's clear primary owning surface.
Classify by the core deliverable, not incidental follow-up: a compiler change that also updates
reference text is `Compiler`, while a ticket whose outcome is correcting false documentation is
`Docs`. Leave Area empty for repository-wide work, tooling outside these four surfaces,
cross-cutting work without one clear owner, or uncertain intake. Do not force the nearest label.

Area is independent of the broad `Bug`, `Improvement`, or `Feature` label and the maintenance theme
in the description. Preserve valid labels outside the current workflow decision. Discovery and
demand may set an evident Area at intake; triage must verify, replace, or clear it from the final
scope. A later technical review does the same when the issue's primary ownership changes.

## Selection

Restrict every listing, deduplication pass, and count to canonical project ID
`077633f0-eb65-4d72-a034-a70615821203` unless the
task explicitly needs a workspace-wide duplicate search.

For automatic work selection, first consider unblocked Todo issues, ordered by priority and then
oldest creation time. Todo is Julia's explicit queue and always outranks Backlog. Fall back to
Backlog only when Todo contains zero issues, not merely zero unblocked issues. If Todo is nonempty
but every issue is blocked, stop and report that the manual queue has no eligible item. When Todo is
empty, select unblocked Backlog issues ordered the same way. A Backlog issue is eligible only when
its description records `Triage disposition: queue-ready`; this durable marker survives later Review
baseline stages and prevents legacy or accidentally misplaced intake from bypassing triage. An
explicitly named queue-ready Backlog issue or a Todo issue overrides automatic selection. A named
Triage issue must pass `silk-triage` before `silk-work` may claim it.

Triage places validated work in Backlog and never promotes it to Todo. Only Julia manually curates
Todo, except that an already-Todo issue may be restored there after an interrupted implementation.
When work claimed from Backlog must return to a queue, return it to Backlog. Recover the pre-claim
queue tier from Linear history; if it is unavailable, use Backlog rather than filling Todo
automatically.

The native Triage inbox became the intake boundary on 2026-09-02. Before selecting from Backlog,
move any preexisting issue whose `Triage disposition` is missing, `intake`, or
`needs-more-investigation` into Triage. Preserve existing Todo as Julia's curated queue; its manual
placement is sufficient for selection even before the durable field is added during a later review.

## Mutations

Re-read an issue immediately before changing its state or specification. Preserve fields unrelated
to the current workflow decision. After a write, read the issue back and verify the intended state.

Follow `REVIEW_BASELINE.md` for issue provenance and currentness. Every newly created issue has a
Source baseline and Review baseline. Every completed technical review updates Review baseline only
after the old-to-new commit delta has been inspected and its conclusion is reflected in the issue.
A field-only mutation, intake comment, or observation of a newer SHA does not advance it.

Do not delete issues. Mark confirmed duplicates as Duplicate and link the survivor. Mark failed
triage as Canceled with a short comment naming the failed claim and the evidence that would reopen
it.
