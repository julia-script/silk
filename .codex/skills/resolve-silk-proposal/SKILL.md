---
name: resolve-silk-proposal
description: Revise a Silk Language Proposal from adversarial findings and record the human author's outcome. Use when the user wants to work through an SLP review, accept or reject proposed changes or splits, return a proposal to review, record an outcome, or hand an accepted direction to OpenSpec.
---

# Resolve a Silk proposal

Read `AGENTS.md`, `proposals/PROCESS.md`, the target proposal, and every unresolved review record
completely before acting. Re-read linked examples from disk.

## Map the resolution

Publish a visible decision plan with exactly one current branch. Group findings that depend on the
same design choice. Before each user-facing decision question, show the synchronized compact
checklist required by `AGENTS.md`.

Lead each branch with the strongest example, the competing models, their consequences, and a
recommendation. Ask one consequential question when repository evidence cannot choose. Keep the
session conversational rather than replaying raw comments.

Completion: every material finding has a proposed response and every human judgment is isolated.

## Revise

Apply the author's decisions to examples and semantics together. For each finding, record one of:

- revised, with the affected section or example;
- delegated to OpenSpec, with why it cannot reverse the direction;
- rejected, with a direct rationale; or
- split, with linked child theses and dependency order.

When splitting, assign new SLP numbers, create linked Drafts from the template, preserve the source
proposal history, and update the index. Change terminal status only when the author explicitly
selects it.

Increment the proposal revision once for the coherent revision, update dates and revision history,
link the review record, and complete its `Revision response` and `Next state` sections.

Completion: no review finding is silently dropped and the revised examples, thesis, scope, and
compiler/standard-library boundary agree.

## Record outcome or return to review

Keep the SLP in Candidate when another adversarial round is warranted. Record Accepted direction,
Deferred, Declined, Withdrawn, or Superseded only from the author's explicit decision, including its
rationale and any revisit/supersession links.

After Accepted direction, create OpenSpec changes only when the author also requests handoff. Carry
over the accepted invariants, decisions, rejected alternatives, affected specs, falsifiers, and
capability-level slices; let OpenSpec own normative deltas and tasks. Link each change to the exact
SLP revision, digest, and slice it realizes, then route the completed planning artifacts through
`$audit-silk-openspec` before implementation.

Completion: the proposal has either a reviewable next Candidate revision or an author-selected
outcome with a durable resolution.
