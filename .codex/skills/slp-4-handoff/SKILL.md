---
name: slp-4-handoff
description: "SLP step 4 of 6. Create the OpenSpec change(s) that realize an SLP in Accepted direction, then audit them for fidelity, normative completeness, and task/verification coverage before implementation. Manual-only: use ONLY when the user explicitly invokes $slp-4-handoff or says \"hand off this SLP to OpenSpec\". Never trigger from generic OpenSpec requests."
---

# SLP 4: Handoff

Pipeline: 1 develop → 2 review → 3 resolve → **4 handoff** → 5 implement → 6 audit-implementation.

Read `AGENTS.md`, `proposals/PROCESS.md` (§ OpenSpec handoff, § Traceability gates),
`OPENSPEC-AUDIT-TEMPLATE.md`, and the target SLP before acting. Require
`Status: Accepted direction`; otherwise stop and point to `$slp-3-resolve`. `--audit-only` skips
creation and audits the existing linked change(s).

## Plan the slices

Take the slices from the SLP's `OpenSpec realization map`. One OpenSpec change per capability-level
slice, in dependency order. Propose the slice list and names; confirm with the author only if the
map is ambiguous or missing.

## Create each change

Use `$openspec-propose` mechanics (`openspec new change`, `openspec instructions ...`, store flags
as applicable). Carry over, verbatim where possible:

- selected model and invariants;
- closed decisions and rejected alternatives (with why they lost);
- affected current specs;
- falsifiers and acceptance blockers as verification requirements;
- driving, boundary, and cross-feature examples as scenarios;
- the compiler/standard-library boundary as a design constraint.

Do not carry task lists or file plans from the SLP; OpenSpec owns normative deltas and tasks.
OpenSpec may refine mechanics but may not reverse the accepted thesis or add compiler privilege.

Each change's `proposal.md` links the SLP path, revision, digest, and slice it realizes. Set the
SLP's `OpenSpec handoff` field to the change list.

## Audit each change

Freeze: run `openspec status --change <name> --json` and `openspec instructions apply --change
<name> --json`; read every artifact and every canonical spec touched by a delta. Record SLP
revision/digest and artifact digests in the next `proposals/NNNN-slug/audits/openspec-<change>-oNNN.md`.
Run `openspec validate <change> --strict --json --no-interactive` and keep the result.

Spawn three fresh agents in parallel (raw artifacts, findings only with exact references):

1. **SLP fidelity** — map model, invariants, examples, falsifiers, interaction map, and privilege
   split to requirements; find omissions, inventions, contradictions, scope drift.
2. **Normative completeness** — do requirements/scenarios cover success, typed failure, invalid
   programs, diagnostics, ownership/Effects interactions, target parity, costs where material?
3. **Realization coverage** — map every scenario to design, tasks, tests, docs, artifacts; find
   orphans both ways.

A checked task or validation pass proves existence, not semantic coverage.

## Fix and gate

Classify findings: **OpenSpec revision required**, **SLP decision required**, **Editorial**.
Apply OpenSpec revisions and editorial fixes directly to the change artifacts, then re-validate —
at most **2** fix passes; no re-spawning of lenses. Route SLP decisions to `$slp-3-resolve` and
stop. `Result: Ready` only when every accepted decision is covered, every observable behavior has
scenarios and verification, artifacts agree, the privilege boundary holds, and strict validation
passes. Otherwise record the open items in the audit record.

Next: `$slp-5-implement`.
