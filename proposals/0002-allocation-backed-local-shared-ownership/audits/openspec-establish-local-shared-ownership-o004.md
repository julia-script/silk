# OpenSpec audit o004: establish-local-shared-ownership

SLP: `proposals/0002-allocation-backed-local-shared-ownership/proposal.md`
SLP revision: 6
SLP digest: `c97959718e551d9d4c4273e6503a18630696c6ac969087192bc3e5133c4ca069`
OpenSpec change: `establish-local-shared-ownership`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `b9399e03673ba9cf76a3aaed652b32793349b0b8bda5bafea77ccfa94a12a811`
- `proposal.md`: `d159dc49a67e1a01316723a030c57c0a52c6ae76467f6fc545e2b876a5812ec1`
- `design.md`: `11794cd589b6332f337144cd7f53d58b3f4acc25c76e6b61bb23d06c022c3f73`
- `specs/bootstrap-semantic-facts/spec.md`: `d1bdff4999867969d4d526709a21ff9b6f1212d0dcf993c9d2db9f4371756d20`
- `specs/bootstrap-ownership/spec.md`: `332d02debccf6715d5cb71b69c51d21ecbe7c8359c800e1f6e330fe5bffd771c`
- `tasks.md`: `f28848cd5fcd6162c45f5bcc2fdbeb323677a9f8f465b2316bb18536116b05ac`

Canonical spec baselines:

- `openspec/specs/bootstrap-semantic-facts/spec.md`: `2d6edde8271b03c17d104f1545371ce02c80a06b27116f67b953a7eab503685b`
- `openspec/specs/bootstrap-ownership/spec.md`: `57bc933cc255f9238bc6e1e5adeddf5cdcb7e1533bf639ff63476502bce3eec6`

Date: 2026-08-22
Result: OpenSpec revision required

## Validation evidence

Strict validation passed with no issues. Three fresh reviewers audited the raw o004 baseline. The
SLP-fidelity and normative-completeness lenses were clean with zero Critical, High, or Medium
findings. Realization coverage mapped every scenario and found no orphan task, but found one Medium
verification-precision gap in task 3.1.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| Canonical sealed identity and exact role | Core inspection and specialization | Sealed role, no lanes | 1.1, 1.3, 1.6 | Covered |
| Total affinity algebra and deterministic recovery | Join/reference/parameter/unavailable scenarios | Four outcomes and total precedence | 1.2, 1.4, 1.5 | Covered |
| Same-thread local execution domain, not one execution identity | Future-consumer and same-domain movement scenarios | No execution/fiber/Scheduler id | 1.6, 2.4 | Covered |
| One obligation per live handle | Ownership movement, aggregate, frame, recovery scenarios | Handle-level affine ownership | 2.1–2.5 | Covered |
| No compiler privilege by source spelling | Five-name ordinary-source scenario | Sealed intrinsic boundary | 3.1 | Revision required: assertions are imprecise |
| Determinism and repository readiness | Normative encodings and repo obligations | Canonical ordering and narrow scope | 3.2–3.3 | Covered |

## Completeness findings

### Missing normative behavior

None.

### Missing boundary or failure scenarios

None.

### Missing implementation or verification work

1. **OpenSpec revision required — exact privilege assertions.** Task 3.1 says the five ordinary
   nominals must not receive “affine-core behavior.” That is not a sound discriminator because an
   ordinary nominal without an admitted Copy conformance can itself be affine. The task must assert
   for every name that analysis publishes an available ordinary nominal and publishes no intrinsic
   identity, exact `LocalSharedStrong` role, spelling-derived `LocalExecution` affinity, or live
   local-shared obligation.

## Divergence findings

### OpenSpec contradictions or inventions

None.

### SLP decisions requiring reconsideration

None.

## Compiler–standard library boundary

The boundary is normatively sound and the full five-name scenario is present. Only the task's
verification language must use the exact semantic and ownership discriminators.

## Required revisions

1. Replace task 3.1's ambiguous “affine-core behavior” assertion with the exact available ordinary
   nominal, intrinsic identity, `LocalSharedStrong`, `LocalExecution`, and obligation assertions for
   every tested spelling.

## Next state

Revise task 3.1 with `$openspec-update-change`, run strict validation, then run a fresh
`$slp-5-audit-openspec` pass. Do not implement from the o004 baseline.
