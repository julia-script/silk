---
name: slp-4-handoff
description: "SLP step 4 of 6. Create the OpenSpec change(s) that realize an SLP in Accepted direction, carrying its invariants, decisions, and capability slices into proposal/design/specs/tasks. Manual-only: use ONLY when the user explicitly invokes /slp-4-handoff or says \"hand off this SLP to OpenSpec\". Never trigger from generic OpenSpec requests."
---

# SLP 4: Handoff

Pipeline: 1 develop → 2 review → 3 resolve → **4 handoff** → 5 audit-openspec → 6 audit-implementation.

Read `AGENTS.md`, `proposals/PROCESS.md` (§ OpenSpec handoff, § Traceability gates), and the target
SLP before acting. Require `Status: Accepted direction`; otherwise stop and point to `/slp-3-resolve`.

## Plan the slices

Take the slices from the SLP's `OpenSpec realization map`. One OpenSpec change per capability-level
slice, in dependency order. Propose the slice list and names; confirm with the author only if the
map is ambiguous or missing.

## Create each change

Use `/openspec-propose` mechanics (`openspec new change`, `openspec instructions ...`, store flags
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

## Finish

Run `openspec validate <change> --strict --json --no-interactive` for each change. Report the
slices created and anything the SLP left underspecified that the author must settle in OpenSpec.

Next: `/slp-5-audit-openspec` on each change before implementation.
