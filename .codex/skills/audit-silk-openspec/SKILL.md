---
name: audit-silk-openspec
description: Audit an SLP-derived OpenSpec change for conceptual fidelity, normative completeness, internal artifact consistency, and task and verification coverage. Use when the user asks whether OpenSpec proposal, specs, design, and tasks fully and faithfully realize an accepted Silk Language Proposal before implementation.
---

# Audit Silk OpenSpec

Read `AGENTS.md`, `proposals/PROCESS.md`, `proposals/OPENSPEC-AUDIT-TEMPLATE.md`, and the target SLP
completely before acting. This is an independent planning review. Leave the SLP and OpenSpec
artifacts unchanged; record required revisions for a later resolution pass.

## Freeze the planning baseline

Select the OpenSpec change using conversation context, the sole active change, or `openspec list
--json`; ask only when selection remains ambiguous. If a store is named or the work lives in one,
resolve its id with `openspec store list --json` and retain `--store <id>` on every supported command.

Run `openspec status --change <name> --json` and `openspec instructions apply --change <name>
--json`. Use their `changeRoot`, `artifactPaths`, and `contextFiles`; assume no artifact names or
locations. Read every existing planning artifact and every canonical spec modified by a delta. Find
the accepted SLP linked by the change or its `OpenSpec handoff`. A missing or ambiguous SLP link is a
planning blocker for SLP-derived work.

Record the SLP revision and SHA-256 digest plus every artifact path and digest in the next unused
`proposals/NNNN-slug/audits/openspec-<change>-oNNN.md` record. Run `openspec validate <change>
--strict --json --no-interactive` and preserve its result as evidence.

Completion: every authority under review has a fixed path, revision or digest, and validation result.

## Run independent audit lenses

Spawn three agents in parallel with the raw fixed artifacts and no conclusions from another lens.
Require findings with exact contract and artifact references; agents edit no files.

1. **SLP fidelity** — map the selected model, invariants, driving and boundary examples, falsifiers,
   interaction map, and compiler/standard-library split to OpenSpec requirements; find omissions,
   inventions, contradictions, and scope drift.
2. **Normative completeness** — test whether requirements and scenarios specify success, typed
   failure, invalid programs, diagnostics, ownership and Effects interactions, target parity, and
   observable costs wherever the SLP makes them material.
3. **Realization coverage** — map every scenario to design decisions, implementation tasks, tests,
   documentation, generated artifacts, and validation evidence; find orphan tasks and requirements.

Trace both directions. OpenSpec may choose realization details but may not introduce a new programmer
model or compiler privilege. A checked task, a `done` status, or a validation pass proves syntax and
existence only, never semantic coverage.

Completion: every SLP-level observable claim and every OpenSpec normative scenario has one explicit
traceability disposition, including honest gaps.

## Synthesize the gate

Write one audit record from the template. Group duplicate findings by contract and classify them:

- **OpenSpec revision required** — the SLP direction is stable, but requirements, scenarios, design,
  tasks, or evidence are incomplete or inconsistent.
- **SLP decision required** — the OpenSpec model changes the accepted conceptual direction or exposes
  a decision the SLP did not make.
- **Editorial** — clarity improvements that change neither contract nor coverage.

Set `Result: Ready` only when every accepted SLP decision is covered, every observable normative
behavior has scenarios and verification work, all artifacts agree, the privilege boundary holds,
and strict validation succeeds. Otherwise list exact artifact changes and route conceptual findings
back to the SLP. Point OpenSpec-only revisions to `$openspec-update-change` with the audit record as
input; point conceptual decisions to `$resolve-silk-proposal` or `$converge-silk-proposal`. Do not
average reviewer disagreement into readiness.

Completion: the record gives a binary planning gate, complete traceability evidence, and actionable
revisions without changing the artifacts it judged.
