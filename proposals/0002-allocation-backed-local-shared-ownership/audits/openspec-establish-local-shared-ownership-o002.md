# OpenSpec audit o002: establish-local-shared-ownership

SLP: `proposals/0002-allocation-backed-local-shared-ownership/proposal.md`
SLP revision: 6
SLP digest: `c97959718e551d9d4c4273e6503a18630696c6ac969087192bc3e5133c4ca069`
OpenSpec change: `establish-local-shared-ownership`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `b9399e03673ba9cf76a3aaed652b32793349b0b8bda5bafea77ccfa94a12a811`
- `proposal.md`: `fa5569ef32805c5e8ca2723641d7d760bbaeb80c371a61088aa05b9ef90b13a7`
- `design.md`: `12a87315e5eeb5199e9632304a957da34b3279b5602b86bdd32b0acf2562bb34`
- `specs/bootstrap-semantic-facts/spec.md`: `e7a07c3a736ca6267a5193c5442ccccdf7e46632fd96129f11969399ed1d215c`
- `specs/bootstrap-ownership/spec.md`: `3f488815f0c3cfbfde91963cf477f8d78151248ade9b4efd50e2dca809108ddc`
- `tasks.md`: `3b66cfb42620f32e679ec27e74165efb64e468078342a504f5d29cc90bcf9f2e`

Canonical spec baselines:

- `openspec/specs/bootstrap-semantic-facts/spec.md`: `2d6edde8271b03c17d104f1545371ce02c80a06b27116f67b953a7eab503685b`
- `openspec/specs/bootstrap-ownership/spec.md`: `57bc933cc255f9238bc6e1e5adeddf5cdcb7e1533bf639ff63476502bce3eec6`

Date: 2026-08-22
Result: OpenSpec revision required

## Validation evidence

`openspec validate establish-local-shared-ownership --strict --json --no-interactive` passed with
one valid change and no issues. `openspec instructions apply` reported 12 pending implementation
tasks and state `ready`; that schema state does not supersede this semantic audit gate.

Three fresh reviewers read the raw accepted SLP, complete change, and both canonical specs. The
realization-coverage lens found no orphan scenario or task. The fidelity and normative-completeness
lenses independently found the reference-affinity omission and underdefined recovery evidence; the
normative lens also exposed open generic parameters and multi-handle aggregates as falsifiers.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| One sealed `Intrinsic.SharedCore<T>` identity, never recognized by source spelling | Semantic-facts inspection and privileged-looking-name scenarios | Sealed role rather than implementation lanes | 1.1 and 3.1 semantic facts/name-collision tests | Covered |
| Every available handle is affine independently of `T` | Ownership move, read, Copy-conformance, Copy-element, and generic-specialization scenarios | Affine category and phase-owned `OWN0003`/`SEM0083` failures | 1.3 and 2.1–2.2 | Covered |
| Local affinity composes through stored values and executions | Nominal/array/union/callable/Effect/frame scenarios | Recursive affinity join | 1.2–1.4 and 2.3–2.4 | Revision required for references and open parameters |
| Malformed facts preserve causal recovery evidence | Malformed specialization, unavailable join, and unavailable ownership scenarios | Unavailable semantic/ownership outcomes | 1.2 and 2.5 | Revision required for multiple-cause determinism |
| One obligation per live handle, not per container | Single-handle move/storage/suspension scenarios | Handle-level affine obligation | 1.3 and 2.1–2.4 | Revision required for multi-handle aggregate falsifier |
| Parallel transfer is future work; this slice only publishes a semantic fact | Future-consumer inspection scenario | No syntax, eligibility query, diagnostic, MIR, or backend check | 1.4 negative scope verification | Covered |
| Compiler privilege is confined to the sealed intrinsic identity/facts | Name-collision scenario | Ordinary names receive no privilege | 3.1 | Covered |

## Completeness findings

### Missing normative behavior

1. **OpenSpec revision required — reference and borrowed-view affinity.** The SLP forbids a local
   handle or retained reference from crossing threads, but the semantic requirement composes only
   nominals, arrays, unions, executable environments, and frames. It does not define the affinity of
   `&Intrinsic.SharedCore<T>`, `&mut Intrinsic.SharedCore<T>`, borrowed views, or an executable that
   captures one. A reference to a local core can therefore become incorrectly `Unrestricted`.
2. **OpenSpec revision required — open generic parameters.** A canonical resolved type parameter is
   neither a concrete unrestricted type nor a diagnostic recovery failure. The three-outcome design
   does not define its symbolic affinity before specialization, and the existing generic-core case
   cannot falsify an incorrect unrestricted default because the outer core is always local.
3. **OpenSpec revision required — multi-handle aggregation.** Every ownership scenario contains one
   core. A conforming implementation could therefore publish one obligation per container instead
   of one per live handle. A nominal/array/executable containing two cores must retain two distinct
   obligations while an active union retains only its selected member's obligations.

### Missing boundary or failure scenarios

1. **OpenSpec revision required — deterministic unavailable evidence.** The requirement says causal
   evidence, the design says one cause, and the task says causes. No scenario determines what happens
   when several components are unavailable. The contract must retain all distinct causal diagnostic
   identities in canonical traversal order or choose another exact deterministic rule.

### Missing implementation or verification work

Tasks need explicit tests for reference/borrowed-view capture, parameter-dependent affinity and
specialization, multiple unavailable causes in canonical order, and multiple handle obligations in
one aggregate/executable. Existing deterministic goldens and full repository gates are otherwise
complete.

## Divergence findings

### OpenSpec contradictions or inventions

The inspection scenario and task require local-affinity “provenance” without defining its shape or
selection for multiple local contributors. This is an invented observable representation not needed
by SLP-0002. Either define it normatively or remove it while retaining the canonical affinity fact.

### SLP decisions requiring reconsideration

None. All findings refine the delegated semantic-fact representation without reversing the accepted
local, non-transferable ownership direction.

## Compiler–standard library boundary

The sealed identity, role, affinity, and ownership fact remain the smallest compiler-owned semantic
surface for this slice. No source actor is recognized by spelling, and the change still introduces
no allocation, lifecycle operation, transfer syntax, parallel policy, or backend representation.

## Required revisions

1. Define affinity for references, borrowed views, and their executable captures, with a falsifying
   scenario and task.
2. Add a symbolic parameter-dependent affinity outcome and deterministic specialization rule, or
   specify another sound non-concrete model with equivalent tests.
3. Specify deterministic multiple-cause unavailable evidence and remove or fully define local
   provenance.
4. Add a scenario and task proving two contained handles produce two obligations, including the
   active-union boundary.

## Next state

Revise the existing OpenSpec artifacts with `$openspec-update-change`, run strict validation, then
run a fresh `$slp-5-audit-openspec` pass. Do not implement this change from the o002 baseline.
