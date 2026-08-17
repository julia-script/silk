---
name: audit-silk-implementation
description: Audit a completed Silk feature implementation against its accepted SLP and OpenSpec contract, classify divergences, draft justified SLP amendments, or request implementation changes. Use after implementation and before OpenSpec archive when the user wants evidence that code, tests, documentation, generated artifacts, compiler privilege, and observable behavior match the agreed direction.
---

# Audit Silk implementation

Read `AGENTS.md`, `proposals/PROCESS.md`, `proposals/IMPLEMENTATION-AUDIT-TEMPLATE.md`, the target
SLP, its OpenSpec planning audit, and every OpenSpec apply `contextFiles` artifact completely before
acting. Review only; edit no implementation code and mark no task complete.

## Freeze the conformance baseline

Select the OpenSpec change as in its local OpenSpec skills. Resolve stores when applicable. Run
`openspec status --change <name> --json` and `openspec instructions apply --change <name> --json`,
then read every returned context file from disk. Require complete tasks; if tasks remain, report the
audit as premature and identify the unfinished contract surface.

Resolve the implementation fixed point from an explicit commit, the change's implementation history,
or a defensible merge-base. If more than one fixed point remains plausible, ask the user. Inspect all
changed code, tests, documentation, generated artifacts, exports, and build inputs plus any unchanged
code claimed as the realization. Record paths and SHA-256 digests for the SLP and OpenSpec artifacts.

Create the next unused `proposals/NNNN-slug/audits/implementation-<change>-iNNN.md` from the template.
Run the repository's required checks and focused behavioral cases in proportion to the feature. A
passing suite is evidence only for the behavior it actually exercises.

Completion: the review has a fixed conceptual contract, normative contract, implementation surface,
and executable evidence set.

## Run independent conformance lenses

Spawn three agents in parallel with raw artifacts, the fixed implementation diff or path set, and
test evidence. Each returns findings only and edits no files.

1. **Language behavior** — execute or trace the SLP's desired, observable, boundary, and cross-feature
   examples; compare semantics, diagnostics, ownership, Effects, services, and target behavior.
2. **OpenSpec conformance** — map each requirement and scenario to code and tests, verify every task's
   claimed result, and find unimplemented, unverified, or extra behavior.
3. **Architecture and privilege** — challenge the actual compiler/standard-library split, search for
   spelling-based compiler knowledge or excess primitives, and inspect cost, generated artifacts,
   public API, and target parity.

Trace every normative contract to both implementation and verification. Inspect tests for asserted
semantics rather than matching names. Treat omitted behavior and undocumented extra behavior as
findings even when all checks pass.

Completion: every SLP and OpenSpec contract has a conforming or divergent disposition supported by
code and behavioral evidence.

## Classify each divergence

Use the traceability classifications in `proposals/PROCESS.md`:

- Record realization refinements without changing the SLP.
- Route OpenSpec gaps to a planning revision and re-audit before archive.
- Treat an implementation departure as justified at SLP level only when real cases, repository
  constraints, a prototype, or observable evidence support a necessary or materially better
  conceptual model. Convenience and already-written code are no justification.
- Request exact implementation changes for unjustified departures.
- Stop on a value, taste, or evidence fork and present one compact author decision.

When objective evidence justifies an SLP-level change, draft the amendment in place: increment its
revision, set `Status: Candidate`, update current/desired/boundary examples, semantic sketch,
interaction map, compiler/standard-library boundary, alternatives, risks, and revision record, then
link the implementation audit. This invocation authorizes drafting the reconsideration, not choosing
its outcome. Mark the OpenSpec change as requiring reconciliation and another planning audit before
the implementation can be conformant.

Completion: every mismatch names the authority that must change and why; justified conceptual
changes produce a reviewable Candidate rather than a retroactive acceptance.

## Write the gate result

Complete the audit record with one result:

- **Conformant** — all contracts trace to implementation and evidence, no unresolved divergence
  remains, required checks pass, and the minimal-privilege boundary holds.
- **Planning amendment required** — the implementation may be viable, but OpenSpec must be corrected.
- **SLP amendment drafted** — conceptual evidence produced a new Candidate revision.
- **Implementation changes required** — list precise expected behavior, affected locations, and
  verification needed without implementing the fixes.
- **Author decision required** — present the competing models, strongest cases, exact tradeoff, and
  recommendation.

Report failures from required checks exactly and distinguish them from conformance findings. Archive
is ready only after a Conformant audit. Route planning amendments through `$openspec-update-change`
and another `$audit-silk-openspec`; route a drafted SLP amendment through proposal review or
convergence; route implementation requests through revised OpenSpec tasks and `$openspec-apply-change`.
