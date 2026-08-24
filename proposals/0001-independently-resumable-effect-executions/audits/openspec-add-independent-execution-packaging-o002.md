# OpenSpec audit o002: add-independent-execution-packaging

SLP: `proposals/0001-independently-resumable-effect-executions/proposal.md`
SLP revision: 31
SLP digest: `963a7420f16bce3bd0ec50acd906b4a4ee43319d132c5452d3686cde643c5635`
OpenSpec change: `add-independent-execution-packaging`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `b9399e03673ba9cf76a3aaed652b32793349b0b8bda5bafea77ccfa94a12a811`
- `proposal.md`: `142014b54210a602a96cee1f28d87a88ca860efe76f93d1924680f6a2c3d01de`
- `design.md`: `7defc6bc6c35fcc6c90277c3ed246453cc604e8a34a6ac620cad3fefbc1813b8`
- `specs/bootstrap-independent-execution-packaging/spec.md`: `caf0f0079494a1493dfe4d669ae8cae42b14eec3d1741d0a2d05b7be579d0194`
- `specs/bootstrap-intrinsic-boundary/spec.md`: `580d56c4619678e5496c5ead18b86dedd634b9110542bb03caea11aab75c35c1`
- `specs/bootstrap-owned-allocation/spec.md`: `4412f6f4a7a6df48995b981909e9ac9e78b6858b61568611b89967a8598898d0`
- `specs/bootstrap-ownership/spec.md`: `2d56d4f7fac821d29a8b6e7fdba49ad2cb48266e62ccc216938011a07789ce91`
- `specs/bootstrap-target-layout/spec.md`: `652606ca7176d49ee2500fd645f43a59b179475e13403ece76b9ed75b9587940`
- `tasks.md`: `4b822f5f7d7b272a95f5174a3af73c3e76ef580e36541cb594ab380269318438`

Canonical spec baselines:

- `openspec/specs/bootstrap-intrinsic-boundary/spec.md`: `1c3360e4eb0b8a9e2ec85b41f0ca22e65a8171e8380cac5aab7a7095bdc9b47f`
- `openspec/specs/bootstrap-owned-allocation/spec.md`: `7fdb142de7c8e61b998bf4fb6e6e58d9b3d7122369b18431bfe8904aeff02a42`
- `openspec/specs/bootstrap-ownership/spec.md`: `eeb6163007fa687f20938dd327b9fc208b6d73c0e1634e45471a7df0109e5beb`
- `openspec/specs/bootstrap-target-layout/spec.md`: `f51eb88256bf3730dae7dd462e09326d5fd5de43e71200a1e66ed74854497d69`

Date: 2026-08-23
Result: Ready

## Validation evidence

- Planning freeze reported complete artifacts, 15 pending tasks, and implementation readiness.
- `openspec validate add-independent-execution-packaging --strict --json --no-interactive`: one
  valid change, zero issues after repair pass 2.
- Three independent lenses were reconciled against the landed `SuspensionOwnership` authority and
  callable syntax. All verified findings were repaired without changing SLP direction.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| One exact caller-funded package | Construction, provenance, refusal scenarios | Plan keyed by A/F/O/R/target/summary | 1.1–2.4 | Covered |
| Intrinsics never allocate | Normative `MUST NOT` allocation contract | Safe source wrapper owns policy | 2.1–2.3 | Covered |
| Drive consumes two affine-capable callbacks | `once fn` contracts and affine-capture scenario | Consuming terminal edges | 3.1–3.3 | Covered |
| Execution-local logical roots | Alternating-owner scenario | Saved per-owner context | 3.4 | Covered |
| One canonical suspension-ownership authority | Cleanup requirements | Extend `SuspensionOwnership` facts/encoding | 3.5 | Covered |
| Later growth remains fatal | Exhaustion scenario | Existing no-unwind stack policy | 3.6 | Covered |

## Completeness findings

### Missing normative behavior

None. The allocation prohibition and `once fn` callable modes are now unambiguous.

### Missing boundary or failure scenarios

None. Affine callback captures, refusal, mismatched provenance, illegal state, cleanup, and fatal
growth are covered.

### Missing implementation or verification work

None. The tasks explicitly extend `SuspensionOwnership`, migrate its consumers/goldens, and include
`pnpm check` plus `pnpm release:candidate`.

## Divergence findings

### OpenSpec contradictions or inventions

None. The change does not create a second frame-ownership model or reusable callback requirement.

### SLP decisions requiring reconsideration

None.

## Compiler–standard library boundary

Ready. Only Layout, unsafe from-Allocation initialization, and callback-shaped drive are privileged;
allocation policy, safe wrappers, schedulers, step sums, and implicit roots remain outside.

## Required revisions

Repair pass 2 corrected the allocation modal, changed drive callbacks to `once fn`, integrated the
canonical suspension-ownership artifact, and completed mandatory gates. No revisions remain.

## Next state

Implementation-ready after `establish-independent-execution-semantics`.
