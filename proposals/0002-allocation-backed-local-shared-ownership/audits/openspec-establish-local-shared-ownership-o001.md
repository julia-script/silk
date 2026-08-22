# OpenSpec audit o001: establish-local-shared-ownership

SLP: `proposals/0002-allocation-backed-local-shared-ownership/proposal.md`
SLP revision: 6
SLP digest: `c97959718e551d9d4c4273e6503a18630696c6ac969087192bc3e5133c4ca069`
OpenSpec change: `establish-local-shared-ownership`
Schema: `spec-driven`
Artifact digests:

- `openspec/changes/establish-local-shared-ownership/.openspec.yaml`: `b9399e03673ba9cf76a3aaed652b32793349b0b8bda5bafea77ccfa94a12a811`
- `openspec/changes/establish-local-shared-ownership/proposal.md`: `a09693c335b8eb8ea0039b74a81834a08d72f2fae59c339a16cfabb5b8dba514`
- `openspec/changes/establish-local-shared-ownership/design.md`: `d5892c38b7ea5c61485c74af4f90c80564e770fb24ce3a8467167196300496eb`
- `openspec/changes/establish-local-shared-ownership/specs/bootstrap-semantic-facts/spec.md`: `83bd07dd88807ef3a193b91e905b3040fe0c91ee479c03c15a7c8609dbfa206c`
- `openspec/changes/establish-local-shared-ownership/specs/bootstrap-ownership/spec.md`: `3a6a98a1b0c6a6048cf84822847d07c086330375a3d68f234fac96d1610c1022`
- `openspec/changes/establish-local-shared-ownership/tasks.md`: `66dbcf3c5885b5dd1da9bf24f4af7bd0defabf51afc0cd1117e3104713d6c40a`

Canonical spec baselines:

- `openspec/specs/bootstrap-semantic-facts/spec.md`: `2d6edde8271b03c17d104f1545371ce02c80a06b27116f67b953a7eab503685b`
- `openspec/specs/bootstrap-ownership/spec.md`: `57bc933cc255f9238bc6e1e5adeddf5cdcb7e1533bf639ff63476502bce3eec6`

Date: 2026-08-22
Result: OpenSpec revision required

## Validation evidence

- The change proposal links unambiguously to SLP-0002 revision 6 and records the matching digest at
  `openspec/changes/establish-local-shared-ownership/proposal.md:7-9`.
- `openspec status --change establish-local-shared-ownership --json` reports all four planning
  artifacts complete.
- `openspec instructions apply --change establish-local-shared-ownership --json` reports state
  `ready` with 0 of 10 implementation tasks complete.
- `openspec validate establish-local-shared-ownership --strict --json --no-interactive` passed one
  of one items with no issues.
- Strict validation establishes schema correctness only. It does not close the traceability and
  verification gaps below.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| Slice 1 establishes only local shared ownership and execution-affinity facts (`proposals/0002-allocation-backed-local-shared-ownership/proposal.md:733-742`) | Both delta requirements; later construction, lifecycle, wrapper, backend, and pressure behavior is absent | Non-goals exclude construction, lifecycle, thread APIs, and target representation (`design.md:16-19`) | Tasks cover semantic and ownership integration only | Covered; scope is faithful |
| One sealed opaque generic core with no public representation lanes (`proposal.md:263-278`, `proposal.md:510-517`) | Semantic requirement and “Inspect a local shared core fact” (`specs/bootstrap-semantic-facts/spec.md:3-16`) | Sealed `LocalSharedStrong` role rather than lanes (`design.md:49-53`) | Task 1.1 checks the encoded fact and hidden lanes | Covered |
| Local execution affinity composes through containing values and executables (`proposal.md:310-313`, `proposal.md:434-455`) | Recursive-composition prose plus local-capture and unavailable-evidence scenarios (`specs/bootstrap-semantic-facts/spec.md:5-31`) | Closed three-outcome affinity property and recursive join (`design.md:23-33`) | Tasks 1.2, 1.4, and 2.2 | Revision required: the complete join and current transfer seam are not normatively realizable |
| Every core handle is non-Copy and moving it transfers one obligation without operating on `T` (`proposal.md:275-278`, `proposal.md:477-487`) | Ownership requirement plus move, copy, and Copy-element scenarios (`specs/bootstrap-ownership/spec.md:3-24`) | Always-affine classification independent of `T` (`design.md:39-47`) | Tasks 2.1 and 2.2 | Revision required: invalid read and invalid Copy conformance are assigned to one ambiguous phase/scenario |
| The core participates in specialization (`proposal.md:585-595`) | Generic nominal prose and the canonical generic-specialization requirement, but no core-specific scenario | Identity, category, and affinity remain independent of `T` (`design.md:12-14`, `design.md:39-44`) | Task 1.3 specializes a wrapper over Copy and affine elements | Revision required: task-specific observable behavior lacks a matching scenario |
| Handles may be retained and moved across frames of one local execution (`proposal.md:310-313`, `proposal.md:448-450`) | Executable-retention scenario covers a callable or Effect only (`specs/bootstrap-ownership/spec.md:26-29`) | Recursive affinity names later execution frames (`design.md:25-33`) | Tasks 1.4 and 2.2 mention same-execution movement and executable storage | Revision required: suspension/resumption of one obligation has no scenario |
| Unavailable facts must not fabricate transferability or ownership satisfaction (`proposal.md:703-717`) | Affinity-unavailability scenario plus canonical unavailable-fact rules | `Unavailable` is a closed affinity outcome (`design.md:25-29`) | Task 2.3 checks malformed elements | Revision required: failed `SharedCore<T>` specialization and causal ownership verdict are not specified |
| Ordinary declarations gain no privilege by spelling (`proposal.md:500-514`, `proposal.md:580-583`) | “Ignore a privileged-looking source name” (`specs/bootstrap-semantic-facts/spec.md:23-26`) | Name-based realization is explicitly rejected (`design.md:35-37`) | Task 3.1 checks `Shared` and `SharedCore` collisions | Covered; privilege boundary holds |

## Completeness findings

### Missing normative behavior

1. **OpenSpec revision required — complete the affinity algebra.** The design selects
   `Unrestricted`, `LocalExecution`, and `Unavailable`, but the requirement does not state the full
   precedence rule and has no all-unrestricted case. Add scenarios for unrestricted aggregates,
   local joined with unrestricted, and unavailable joined with known components, including arrays
   and unions named by the requirement. References: `design.md:23-29`,
   `specs/bootstrap-semantic-facts/spec.md:5-31`, `tasks.md:6-8`.
2. **OpenSpec revision required — specialize the new core explicitly.** Add a scenario in which a
   generic wrapper specializes over Copy and affine element types while preserving canonical
   intrinsic identity, the exact role, local affinity, and one affine obligation. References:
   `proposal.md:33-34`, `design.md:39-44`, `tasks.md:9-10`.
3. **OpenSpec revision required — specify malformed element behavior.** Add a failed or unavailable
   `SharedCore<T>` specialization scenario that retains the originating diagnostic cause and does
   not fabricate an unrestricted affinity or satisfied ownership verdict. References:
   `specs/bootstrap-semantic-facts/spec.md:28-31`, `specs/bootstrap-ownership/spec.md:3-9`,
   `tasks.md:21-22`, and canonical `openspec/specs/bootstrap-ownership/spec.md:9-18`.
4. **OpenSpec revision required — cover same-execution suspension.** Add a scenario proving that
   suspension and resumption within one local execution retain local affinity and transfer exactly
   one handle obligation. Count transitions and cleanup remain correctly delegated to later slices.
   References: `proposal.md:16-17`, SLP `proposal.md:310-313` and `proposal.md:448-450`, delta
   `specs/bootstrap-semantic-facts/spec.md:8-11`, and canonical
   `openspec/specs/bootstrap-ownership/spec.md:644-669`.

### Missing boundary or failure scenarios

1. **OpenSpec revision required — align the future transfer boundary.** The change proposal reserves
   transfer rejection for a future parallel model and the design excludes thread APIs and a dormant
   diagnostic, but the delta requires current semantic rejection and task 1.4 assumes an undefined
   “transfer-checking seam.” This slice should publish and verify deterministic local-affinity data
   for a later transfer consumer, or it must define a present non-syntax eligibility query and its
   provenance. It cannot require an unimplemented source boundary. References: `proposal.md:16-17`,
   `design.md:16-19`, `design.md:31-33`, `specs/bootstrap-semantic-facts/spec.md:10-11`,
   `specs/bootstrap-semantic-facts/spec.md:33-36`, and `tasks.md:11-12`.
2. **OpenSpec revision required — split Copy failures by owning phase.** An ordinary duplication
   attempt belongs to ownership; an invalid `Copy` conformance belongs to conformance validation
   before ownership consumes the sealed category. Split the disjunctive scenario and task, and pin
   the owning diagnostic code, primary span, related/cause evidence, and violation or unavailable
   verdict as applicable. References: `specs/bootstrap-ownership/spec.md:16-19`, `tasks.md:16-17`,
   canonical `openspec/specs/bootstrap-ownership/spec.md:80-103`, and canonical
   `openspec/specs/bootstrap-ownership/spec.md:127-148`.

### Missing implementation or verification work

1. **OpenSpec revision required — restore repository handoff gates.** Task 3.3 stops after typecheck,
   Biome, and focused compiler tests. It must also run the required repository-wide `pnpm test` and
   `pnpm check` in the prescribed order, and `pnpm release:candidate` because compiler package
   contents change. The task must report exact failures and whether they predate the change.
   Reference: `tasks.md:30-31` and `AGENTS.md` repository workflow.

## Divergence findings

### OpenSpec contradictions or inventions

- The undefined current transfer rejection is the only scope contradiction. The SLP requires the
  local-affinity fact now but assigns future transfer syntax and parallel rules to a later proposal
  (`proposals/0002-allocation-backed-local-shared-ownership/proposal.md:452-455`,
  `proposal.md:703-717`). No other invented behavior or scope drift was found.
- Editorial: use the exact design role identifier `LocalSharedStrong` consistently in the normative
  requirement and inspection scenario instead of alternating “local-strong-handle role” and
  “local-strong role” (`design.md:49-53`, `specs/bootstrap-semantic-facts/spec.md:5-16`).

### SLP decisions requiring reconsideration

None. Every substantive finding can be resolved inside OpenSpec without changing the accepted SLP.

## Compiler–standard library boundary

The intended boundary is sound. The change makes one intrinsic type identity semantic, hides count,
address, access, layout, and reclaim lanes, and explicitly rejects privilege inferred from ordinary
`Shared` or `SharedCore` declarations. It does not introduce a compiler-known public actor, raw
address surface, allocation policy, lifecycle operation, backend behavior, atomics, or parallel
model. The required revisions above sharpen verification; they do not widen compiler privilege.

## Required revisions

1. State the complete three-outcome affinity join and add scenarios for its base and mixed cases.
2. Add core-specific generic-specialization and unavailable-specialization scenarios with causal facts.
3. Add same-local-execution suspension/resumption coverage for one retained obligation.
4. Replace the undefined current transfer rejection with fact publication for a future consumer, or
   define a present non-syntax eligibility query and verification artifact.
5. Split ordinary duplication from invalid Copy conformance, assign each to its owning phase, and
   verify diagnostic code/spans and verdict data.
6. Make the role spelling consistently `LocalSharedStrong`.
7. Add full `pnpm test`, `pnpm check`, `pnpm release:candidate`, and exact failure reporting to the
   final verification task.

## Next state

Do not implement this change yet. Revise it with `$openspec-update-change`, then re-run
`$slp-5-audit-openspec` against the new artifact digests. No SLP resolution step is required.
