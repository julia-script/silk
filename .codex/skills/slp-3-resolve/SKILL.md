---
name: slp-3-resolve
description: "SLP step 3 of 6. Walk the human author through the review ledger and forks of a Silk Language Proposal, apply their decisions, record the outcome. Manual-only: use ONLY when the user explicitly invokes $slp-3-resolve or says \"resolve this SLP\"."
---

# SLP 3: Resolve

Pipeline: 1 develop → 2 review → **3 resolve** → 4 handoff → 5 implement → 6 audit-implementation.

Read `AGENTS.md`, `proposals/PROCESS.md`, the target proposal, `reviews/ledger.md`, and the latest
review record before acting. Re-read linked examples from disk.

## Map the resolution

Publish a visible decision plan with exactly one current branch. Group open ledger items and forks
by the design choice they depend on. Lead each branch with the strongest example, competing models,
consequences, and a recommendation; ask one consequential question when repository evidence cannot
choose. Medium/Low items are offered as a batch: accept, defer, or drop.

## Revise

Apply decisions to examples and semantics together. Per ledger item record one of: revised (section
or example), delegated to OpenSpec (why it cannot reverse direction), rejected (rationale), deferred,
or split (linked child theses, dependency order). When splitting, assign new SLP numbers, create
linked Drafts, preserve history, update the index.

Increment the revision once, update dates and revision record, close ledger states.

## Record outcome

Author chooses: return to Candidate for another `$slp-2-review` (only when blockers changed
materially), or Accepted direction, Deferred (with `Revisit when`), Declined, Withdrawn, Superseded.
Record rationale. Never assign an outcome on the author's behalf.


Next: `$slp-4-handoff`.
