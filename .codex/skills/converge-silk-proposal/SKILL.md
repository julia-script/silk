---
name: converge-silk-proposal
description: Autonomously repeat adversarial review and revision of a Candidate Silk Language Proposal until agent convergence or an irreducible decision fork is isolated. Use only when the user explicitly asks the agent to run the full convergence loop without manual review/resolve calls between rounds.
---

# Converge a Silk proposal

Read `AGENTS.md`, `proposals/PROCESS.md`, `proposals/REVIEW-TEMPLATE.md`, the target Candidate, all
linked examples, and prior unresolved reviews completely before acting. This is an explicit,
potentially long-running workflow; never infer authorization from the mere existence of a proposal.

Publish a visible plan for the convergence loop with exactly one current branch. Keep the user
updated between rounds without asking them to orchestrate routine review and revision.

## Run a round

1. Freeze the current revision and SHA-256 digest.
2. Spawn three fresh agents in parallel for the process's scope/coherence, examples/programmer model,
   and compiler privilege/realizability lenses. Give them raw artifacts and no other reviewer's
   conclusions. Tell them to return findings only and edit no files.
3. Synthesize findings into the next unused review record. Group by design claim and separate
   blockers, edits, safe realization questions, and reviewer disagreements.
4. If the round is clean, increment the consecutive-clean count and run another fresh round without
   altering the proposal.
5. If material findings exist, reset the clean count, revise the proposal using repository evidence
   and accepted Silk constraints, repair examples before closing objections with prose, increment
   `Revision`, and complete the review response.

For a material split, either defend cohesion against the concrete split or create linked Draft
children with distinct theses, driving examples, and dependency order. Treat the user's invocation
as authorization to develop those derived Drafts and converge them after they meet the Candidate
bar, but assign no outcome status on the author's behalf. Preserve the source proposal and index
links.

Completion: the round has one durable review record, and every material finding has changed the next
revision or has a direct evidence-backed rebuttal.

## Detect convergence

Declare agent convergence only after two consecutive fresh rounds find no unresolved material
proposal blocker. A material finding can change thesis, scope, programmer model, semantics, examples,
or the compiler/standard-library boundary. Editorial suggestions and safely delegated OpenSpec
questions do not reset the clean count.

Leave every converged artifact in Candidate. Record the two clean rounds in its review metadata and
report why the proposal converged. Agent convergence is not author acceptance.

Completion: two independent clean rounds are recorded for every Candidate in scope.

## Stop on a real fork

Track foundational objections across rounds. Stop only when the same objection survives three
consecutive revisions, the choice depends on the author's values or taste, or named prototype or
research evidence is missing.

Return one compact decision fork with the competing models, strongest current/desired/boundary
examples, exact unresolved tradeoff, attempted revisions, and a recommendation. Show the visible
decision checklist before asking the author. Do not return a raw review dump or manufacture consensus
by averaging agents.

Completion: the author receives exactly the judgment or evidence request the autonomous loop cannot
resolve.
