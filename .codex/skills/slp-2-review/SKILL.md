---
name: slp-2-review
description: "SLP step 2 of 6. Run the bounded adversarial review loop (max 3 rounds, evidence-gated, ledger-tracked) on a Candidate Silk Language Proposal and stop with a recorded reason. Manual-only: use ONLY when the user explicitly invokes $slp-2-review or says \"review this SLP\". Never trigger from generic review requests."
---

# SLP 2: Review

Pipeline: 1 develop → **2 review** → 3 resolve → 4 handoff → 5 implement → 6 audit-implementation.

Read `AGENTS.md`, `proposals/PROCESS.md` (§ Bounded adversarial review), `REVIEW-TEMPLATE.md`,
`LEDGER-TEMPLATE.md`, the target Candidate, linked examples, and any existing ledger before acting.
Target must be `Status: Candidate`; otherwise report Candidate-bar gaps and stop.

Default is the full loop. `--single` runs one round and stops. The loop is physically bounded by
this skill, not by critics: **you own continuation; critics only propose.**

## Round 0: freeze the contract

Record revision, SHA-256 digest, thesis, in-scope claims `C1..Cn`, and explicit out-of-scope items
in the next `reviews/rNNN.md`. Create `reviews/ledger.md` if absent. Keep a visible plan showing
round `N of 3` and the open blocker count.

## Each round (max 3)

1. **Critique.** Spawn three fresh agents in parallel, one lens each (scope/coherence,
   examples/programmer model, compiler privilege/realizability). Give each the proposal path,
   digest, contract, the current ledger, and repository access. Require structured findings:
   `id | claim Cn | severity | evidence | new or duplicate-of`. Findings only, no edits.
2. **Verify.** For each finding check the evidence yourself against proposal, examples, and repo.
   Assign severity by consequence (table in PROCESS). Mark `REJECTED` (evidence fails),
   `DUPLICATE`, `OUT_OF_SCOPE`, or `VERIFIED`. Update the ledger; never let a rephrasing become a
   new id.
3. **Decide.** Blockers = open ∧ verified ∧ in-scope ∧ Critical/High ∧ not duplicate.
   - No blockers → **Clean**, stop.
   - Same id reopened twice, or a blocker contested with no new evidence → **Fork**, stop.
   - No new verified blocker this round but one still open → **No progress**, stop.
   - Round 3 done → **Cap**, stop.
4. **Revise.** Otherwise fix only the blockers: examples first, then prose. Increment `Revision`,
   fill `Revision response`. If the revision introduces a new blocker, roll back to the prior
   revision and treat as Fork. Freeze the new digest and start the next round.

Medium/Low findings are recorded in the ledger and never extend the loop.

## Stop

Write `Stop reason` and `Next state` in the round record; set `Review state` in the proposal
(`Clean rN` | `Fork: <id>` | `No progress` | `Cap`). Leave `Status: Candidate`. For Fork, present one
compact decision: competing models, strongest current/desired/boundary examples, exact tradeoff,
recommendation. Do not score, vote, approve, or average disagreements away.

Next: `$slp-3-resolve`.
