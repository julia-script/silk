# OpenSpec audit o002: add-independent-execution-engine-parity

SLP: `proposals/0001-independently-resumable-effect-executions/proposal.md`
SLP revision: 31
SLP digest: `963a7420f16bce3bd0ec50acd906b4a4ee43319d132c5452d3686cde643c5635`
OpenSpec change: `add-independent-execution-engine-parity`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `b9399e03673ba9cf76a3aaed652b32793349b0b8bda5bafea77ccfa94a12a811`
- `proposal.md`: `e871fda8995a87656d76ad27ef2769be490546e42afd83a991313b0de79acc07`
- `design.md`: `896be546c7a64c15035c2ead537e297d6101b144c1e48bdca40d14f502144e4e`
- `specs/bootstrap-backend/spec.md`: `2a9dad4ce8e9e06bbff841487463624c8debef9c81947b6dad8f7172e7e47936`
- `specs/bootstrap-evaluation/spec.md`: `6f8c9b00222c5eb9c283dc5ef8eff9e220b07d1c4c2705ec013f2472ab584d51`
- `specs/bootstrap-independent-execution-engine-parity/spec.md`: `144ab93da5eb92dfc463f957ca8cce32af49ba41947ae0c253d7417be0af402d`
- `tasks.md`: `f76482a770d9bf4b93468eac64f633ed1aeecd2136c84e69cce39c285cdce6b5`

Canonical spec baselines:

- `openspec/specs/bootstrap-backend/spec.md`: `7b6fd0eae33a4743baad87a32bf432a095936284df447e3d2d080c43d370f9a6`
- `openspec/specs/bootstrap-evaluation/spec.md`: `e5a125ed11842aadcbd33c8a347857bc80054c76d0d5b5099fbf2f0f66bd48e5`

Date: 2026-08-23
Result: Ready

## Validation evidence

- Planning freeze reported complete artifacts, 15 pending tasks, and implementation readiness.
- `openspec validate add-independent-execution-engine-parity --strict --json --no-interactive`:
  one valid change, zero issues after repair pass 2.
- Three independent lenses found no engine-level SLP fork. The verified realization gap—possible
  parallel ownership models—was repaired by assigning both backends the canonical
  `SuspensionOwnership` authority.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| Verified MIR owns lifecycle authority | MIR and negative scenarios | Validated target-neutral transitions | 1.1–1.3 | Covered |
| Evaluator is deterministic oracle | Alternating owners and event scenarios | Explicit records, no host async identity | 2.1–2.3 | Covered |
| Native and Wasm share suspension ownership facts | Cross-engine parity requirements | Canonical `SuspensionOwnership` input | 3.1–3.2 | Covered |
| Local Wake adds no transfer/atomic policy | Reactor scenarios | Same-thread delivery | 3.3, 4.3 | Covered |
| Nested/LIFO path remains cheap | Nested regression scenarios | Existing lowering foundation | 3.4 | Covered |
| Emission remains deterministic | Repeat-emission scenario | Verified plan plus private helpers | 3.5 | Covered |

## Completeness findings

### Missing normative behavior

None.

### Missing boundary or failure scenarios

None. Invalid MIR, all wake/cancel branches, target unavailability, illegal drive, and no-unwind
stack exhaustion are explicit.

### Missing implementation or verification work

None. Native and Wasm must consume the same canonical ownership encoding, and the full repository
and release-candidate gates are mandatory.

## Divergence findings

### OpenSpec contradictions or inventions

None. No scheduler, worker, timer policy, thread transfer, or second ownership authority is added.

### SLP decisions requiring reconsideration

None.

## Compiler–standard library boundary

Ready. Engines lower verified general execution/wake operations; physical storage and helpers stay
private, while source actors and parallel policy remain outside the compiler.

## Required revisions

Repair pass 2 anchored both backend plans in `SuspensionOwnership` and added the required
release-candidate gate. No revisions remain.

## Next state

Implementation-ready after `add-external-wake-parking`.
