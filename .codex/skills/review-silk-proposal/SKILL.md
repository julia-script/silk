---
name: review-silk-proposal
description: Run one fixed-revision adversarial review of a Candidate Silk Language Proposal. Use when the user asks to review, challenge, pressure-test, or find scope and compiler-privilege problems in an SLP before deciding or revising it.
---

# Review a Silk proposal

Read `AGENTS.md`, `proposals/PROCESS.md`, `proposals/REVIEW-TEMPLATE.md`, and the target proposal
completely before acting. Formal review targets `Status: Candidate`; if another status is supplied,
report the Candidate-bar gaps instead of fabricating a formal round.

## Freeze the round

Read linked examples and prior reviews. Record the proposal revision and SHA-256 digest. Choose the
next unused `reviews/rNNN.md` path. Leave the proposal unchanged during the round.

Completion: the target revision, digest, and review path are fixed.

## Run independent lenses

Spawn three agents in parallel. Give each the proposal path, fixed revision/digest, process path, and
repository access. Tell each to return findings only and edit no files. Do not show one reviewer the
other reviewers' conclusions.

Assign exactly one lens per agent:

1. **Scope and language coherence** — attempt to split the thesis, find implicit special cases,
   challenge invariants, and propose a simpler subtractive model.
2. **Examples and programmer model** — reason from the cases, find missing counterexamples and
   interactions, and test diagnostics, learning, and cognitive cost.
3. **Compiler privilege and realizability** — shrink the intrinsic, move policy into ordinary Silk,
   and attack target neutrality, cost transparency, and feasibility assumptions.

Require concrete counterexamples or counterproposals for material findings. A scope finding includes
child theses, driving cases, and dependency order.

Completion: all three independent lenses have returned or a precise reviewer failure is recorded.

## Synthesize

Group findings by underlying design claim rather than reviewer. Distinguish:

- proposal-level blockers that can change thesis, scope, semantics, examples, or privilege boundary;
- editorial improvements;
- safe OpenSpec realization questions; and
- genuine reviewer disagreements.

Write one review record from the template. Mark `Result: Clean` only when no proposal-level blocker
remains. Do not score, vote, approve, revise the dossier, or average disagreements away.

Completion: the durable review record makes every material objection actionable and identifies the
next manual resolution or convergence step.

