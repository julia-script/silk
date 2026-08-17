---
name: develop-silk-proposal
description: Develop or revise a Silk Language Proposal through a conversational, example-driven design workbench. Use when the user brings a high-level language or standard-library goal, wants to discover the missing language capability beneath a desired program, or wants to evolve an unfinished SLP Draft toward Candidate.
---

# Develop a Silk proposal

Read `AGENTS.md`, `proposals/PROCESS.md`, and `proposals/TEMPLATE.md` completely before acting. Treat
the process file as authoritative. Read the target proposal and its linked artifacts again from disk
whenever continuing an existing Draft.

## Establish the workbench

Publish a visible decision plan with exactly one current branch. Keep it synchronized as the design
changes. Use the repository's required compact checklist before each user-facing decision question.

When no proposal exists:

1. Search `proposals/` for overlapping work.
2. Assign the next unused four-digit SLP number.
3. Create `proposals/<NNNN>-<slug>/proposal.md` from the template and add it to
   `proposals/README.md`.
4. Seed the Draft immediately with the user's real goal, one provisional desired example, and honest
   unknowns. Do not wait to finish an interview.

Completion: one stable Draft path exists and the first desired case makes the high-level goal
observable.

## Discover the actual feature

Work backward from the desired program:

```text
real activity -> desired source -> current blocker -> missing capability
              -> smallest compiler primitive + ordinary-Silk public model
```

Inspect current code, specs, standard-library source, and pressure programs before describing current
behavior. Show the first blocked expression or semantic operation. Distinguish the user's desired
feature from the lower-level capability Silk must gain.

Bound the first pass at the evidence needed to establish that blocker and a provisional thesis.
Create the initial Draft before exhaustively tracing every downstream compiler phase; expand the
interaction analysis as the conversation makes each surface material.

Offer candidate models, examples, tradeoffs, and a recommendation before asking a focused question.
Make critique reciprocal: challenge the user's assumptions and expose your own model for them to
reject. Follow interesting objections rather than walking template headings in order.

Completion: the Draft names one provisional thesis and demonstrates why the current language cannot
express the driving case.

## Evolve through examples

Maintain paired current/desired examples throughout the conversation. For every central behavior,
show intent, current Silk, desired Silk, observable result, and a boundary case. Mark illustrative
syntax. Add cross-feature examples when ownership, Effects, services, generics, modules, or targets
matter.

Update the Draft after substantive decisions. Preserve rejected directions and why they lost. Leave
unresolved areas explicit instead of completing them with plausible prose.

Re-run the compiler/standard-library privilege audit whenever the public model changes. Surface scope
pressure when independent theses appear; sketch a concrete split and let the author split or add a
scope-cohesion argument.

Completion: every current central claim is constrained by an example, and the prose and examples do
not contradict one another.

## Prepare Candidate

Keep `Status: Draft` until the author explicitly chooses Candidate. Before changing status, apply
every item in the Candidate bar from `proposals/PROCESS.md`. Report unmet items as the remaining
design frontier. Increment `Revision`, update dates and the revision record, and update the index
when promotion succeeds.

Do not create OpenSpec changes during development. Candidate means ready for adversarial conceptual
review, not ready for implementation.
