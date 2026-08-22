---
name: slp-1-develop
description: "SLP step 1 of 5. Create or evolve a Draft Silk Language Proposal through an example-driven design conversation, up to the Candidate bar. Manual-only: use ONLY when the user explicitly invokes /slp-1-develop or says \"develop/draft an SLP\". Never trigger from a general language or design question."
---

# SLP 1: Develop

Pipeline: **1 develop** → 2 review → 3 resolve → 4 handoff → 5 audit-implementation.

Read `AGENTS.md`, `proposals/PROCESS.md`, and `proposals/TEMPLATE.md` before acting. Re-read the
target Draft from disk when continuing one.

## Establish the workbench

Publish a visible decision plan with exactly one current branch; keep it synchronized.

When no proposal exists:

1. Search `proposals/` for overlapping work.
2. Assign the next unused four-digit SLP number.
3. Create `proposals/<NNNN>-<slug>/proposal.md` from the template; add it to `proposals/README.md`.
4. Seed the Draft immediately with the user's real goal, one provisional desired example, and honest
   unknowns. Do not wait to finish an interview.

## Discover the actual feature

Work backward: real activity → desired source → current blocker → missing capability → smallest
compiler primitive + ordinary-Silk public model. Inspect current code, specs, and standard-library
source before describing current behavior. Show the first blocked expression.

Offer candidate models, tradeoffs, and a recommendation before asking one focused question. Make
critique reciprocal. Follow objections rather than walking template headings in order.

## Evolve through examples

Maintain paired current/desired examples; for every central behavior show intent, current Silk,
desired Silk, observable result, and a boundary case. Add cross-feature cases when ownership,
Effects, services, generics, modules, or targets matter. Preserve rejected directions. Leave
unresolved areas explicit. Re-run the privilege audit whenever the public model changes. Surface
scope pressure when independent theses appear and sketch a concrete split.

## Prepare Candidate

Keep `Status: Draft` until the author explicitly chooses Candidate. Before promotion apply every item
in the Candidate bar in `proposals/PROCESS.md`; report unmet items as the remaining frontier.
Increment `Revision`, update dates, revision record, and index. Do not create OpenSpec changes.

Next: `/slp-2-review`.
