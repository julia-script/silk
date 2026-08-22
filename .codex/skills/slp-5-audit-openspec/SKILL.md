---
name: slp-5-audit-openspec
description: "SLP step 5 of 6. Audit an SLP-derived OpenSpec change for fidelity, normative completeness, consistency, and task/verification coverage before implementation. Manual-only: use ONLY when the user explicitly invokes $slp-5-audit-openspec. Never trigger from generic OpenSpec commands."
---

# SLP 5: Audit OpenSpec

Pipeline: 1 develop → 2 review → 3 resolve → 4 handoff → **5 audit-openspec** → 6 audit-implementation.

Read `AGENTS.md`, `proposals/PROCESS.md` (§ Traceability gates), `OPENSPEC-AUDIT-TEMPLATE.md`, and
the target SLP before acting. Review only; edit neither SLP nor OpenSpec artifacts. One pass; no
loop.

## Freeze the baseline

Select the change (context, sole active change, or `openspec list --json`; ask only if ambiguous).
Resolve `--store <id>` when applicable. Run `openspec status --change <name> --json` and
`openspec instructions apply --change <name> --json`; read every artifact and every canonical spec
touched by a delta. A missing or ambiguous SLP link is a blocker. Record SLP revision/digest and
artifact digests in the next `proposals/NNNN-slug/audits/openspec-<change>-oNNN.md`. Run
`openspec validate <change> --strict --json --no-interactive` and keep the result.

## Run three lenses in parallel

Fresh agents, raw artifacts, findings only with exact references:

1. **SLP fidelity** — map model, invariants, examples, falsifiers, interaction map, and privilege
   split to requirements; find omissions, inventions, contradictions, scope drift.
2. **Normative completeness** — do requirements/scenarios cover success, typed failure, invalid
   programs, diagnostics, ownership/Effects interactions, target parity, costs where material?
3. **Realization coverage** — map every scenario to design, tasks, tests, docs, artifacts; find
   orphans both ways.

A checked task or validation pass proves existence, not semantic coverage.

## Write the gate

Classify findings: **OpenSpec revision required**, **SLP decision required**, **Editorial**.
`Result: Ready` only when every accepted decision is covered, every observable behavior has scenarios
and verification, artifacts agree, the privilege boundary holds, and strict validation passes.
Route OpenSpec revisions to `$openspec-update-change` then re-run this skill; route conceptual
findings to `$slp-3-resolve`.

Next: implement via `$openspec-apply-change`, then `$slp-6-audit-implementation`.
