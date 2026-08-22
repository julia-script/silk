---
name: slp-6-audit-implementation
description: "SLP step 6 of 6. Audit a completed implementation against its accepted SLP and OpenSpec contract, classify divergences, and write the archive gate. Manual-only: use ONLY when the user explicitly invokes $slp-6-audit-implementation. Never trigger from generic review or archive requests."
---

# SLP 6: Audit implementation

Pipeline: 1 develop → 2 review → 3 resolve → 4 handoff → 5 audit-openspec → **6 audit-implementation**.

Read `AGENTS.md`, `proposals/PROCESS.md` (§ Traceability gates), `IMPLEMENTATION-AUDIT-TEMPLATE.md`,
the target SLP, its OpenSpec audit, and every apply `contextFiles` artifact before acting. Review
only: no implementation edits, no task checkboxes. One pass; no loop.

## Freeze the baseline

Select the change and store as in step 5. Run `openspec status` and `openspec instructions apply`
(`--json`); require complete tasks, else report premature. Fix the implementation point (explicit
commit, change history, or merge-base; ask if ambiguous). Record SLP/OpenSpec digests in the next
`proposals/NNNN-slug/audits/implementation-<change>-iNNN.md`. Run required checks and focused
behavioral cases; a passing suite is evidence only for what it exercises.

## Run three lenses in parallel

Fresh agents, raw artifacts plus diff and test evidence, findings only:

1. **Language behavior** — execute or trace the SLP's desired, boundary, and cross-feature examples.
2. **OpenSpec conformance** — map each requirement/scenario to code and tests; find unimplemented,
   unverified, or extra behavior.
3. **Architecture and privilege** — challenge the compiler/standard-library split, spelling-based
   compiler knowledge, excess primitives, cost, public API, target parity.

## Classify and gate

Use the PROCESS classifications: realization refinement, OpenSpec gap, justified SLP divergence,
unjustified implementation divergence, author decision fork. Justification needs real cases,
constraints, prototypes, or observable behavior — never the code's mere existence. For a justified
SLP divergence, draft the amendment in place as a new Candidate revision (examples, sketch,
interaction map, boundary, revision record) and link the audit; the author picks its outcome.

Result is one of: **Conformant** | **Planning amendment required** | **SLP amendment drafted** |
**Implementation changes required** | **Author decision required**. Report check failures exactly.
Archive only after Conformant. Routes: planning → `$openspec-update-change` + `$slp-5-audit-openspec`;
SLP amendment → `$slp-2-review`; implementation → `$openspec-apply-change` then rerun this skill.
