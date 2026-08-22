# OpenSpec audit o003: establish-local-shared-ownership

SLP: `proposals/0002-allocation-backed-local-shared-ownership/proposal.md`
SLP revision: 6
SLP digest: `c97959718e551d9d4c4273e6503a18630696c6ac969087192bc3e5133c4ca069`
OpenSpec change: `establish-local-shared-ownership`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `b9399e03673ba9cf76a3aaed652b32793349b0b8bda5bafea77ccfa94a12a811`
- `proposal.md`: `fa5569ef32805c5e8ca2723641d7d760bbaeb80c371a61088aa05b9ef90b13a7`
- `design.md`: `eaa94f98fd655351c1908fe60cc73502e6ac458d967e6859da5e972408bb23cd`
- `specs/bootstrap-semantic-facts/spec.md`: `16c46fdce3cc2cc1f1ceca69dc2a01af8f31db028bd6a2a60a5c89a7f195ea60`
- `specs/bootstrap-ownership/spec.md`: `6b06bbee2d486df9f93b3a82333e769e70baa612b8d49e6cbe493b8a07432c73`
- `tasks.md`: `844adb55b95254d587aeb0d6f7bfc9d8a31099aa42fbe0dea271d6b4d51b8c1f`

Canonical spec baselines:

- `openspec/specs/bootstrap-semantic-facts/spec.md`: `2d6edde8271b03c17d104f1545371ce02c80a06b27116f67b953a7eab503685b`
- `openspec/specs/bootstrap-ownership/spec.md`: `57bc933cc255f9238bc6e1e5adeddf5cdcb7e1533bf639ff63476502bce3eec6`

Date: 2026-08-22
Result: OpenSpec revision required

## Validation evidence

Strict OpenSpec validation passed with no issues, and the apply instructions reported 14 pending
tasks. Three fresh reviewers audited the raw o003 baseline. Normative completeness and realization
coverage were clean: all 21 scenarios mapped to design and explicit verification work, including
references, open parameters, multiple unavailable causes, and multiple handle obligations. The SLP
fidelity lens found two remaining traceability gaps.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| Canonical core identity and `LocalSharedStrong` role | Core inspection, specialization, name-collision scenarios | Sealed role, no representation lanes | 1.1, 1.3, 3.1 | Covered |
| Total affinity algebra, symbolic generics, references, and recovery | Join, parameter, retained-borrow, malformed, multi-cause scenarios | Four outcomes and deterministic join | 1.2, 1.4, 1.5 | Covered |
| One affine obligation per live handle | Move, Copy, aggregate, Effect, recovery scenarios | Affine handle independent of `T` | 1.3, 2.1–2.5 | Covered |
| `LocalExecution` permits same-thread execution movement | Effect suspension/resumption scenario | Same-execution frames only | 2.4 | Revision required: domain versus instance is not explicit |
| No library declaration gains compiler privilege by spelling | Generic prohibition; `Shared`/`SharedCore` case | Sealed intrinsic boundary | 3.1 | Revision required: accepted named falsifiers lack evidence |
| Parallel transfer syntax and rejection belong to a later proposal | Future-consumer inspection scenario | Outcome only, no eligibility or diagnostic | 1.6 | Covered |

## Completeness findings

### Missing normative behavior

1. **OpenSpec revision required — local execution domain versus execution instance.** The SLP allows
   movement between frames of one resumable execution and between fibers owned by one local
   Scheduler. The o003 design and scenarios cover Effect suspension inside one execution but do not
   say that `LocalExecution` is a same-thread domain property rather than an execution-instance id.
   A future consumer could therefore interpret it as confinement to the current execution. Specify
   the domain-level fact and prove that parking and same-domain frame transfer preserve exactly one
   obligation without introducing Scheduler policy in this slice.

### Missing boundary or failure scenarios

None beyond the privilege evidence gap below.

### Missing implementation or verification work

1. **OpenSpec revision required — complete name-spelling falsifier.** The requirement is generic, but
   the scenario and task exercise only `Shared` and `SharedCore`. The accepted SLP explicitly names
   `Deferred` and `Scheduler` as library actors the compiler must not recognize. Add them and one
   unrelated privileged-looking ordinary name to the semantic test matrix.

## Divergence findings

### OpenSpec contradictions or inventions

None.

### SLP decisions requiring reconsideration

None. The local-domain clarification and expanded privilege matrix realize accepted constraints
without adding Scheduler syntax or parallel-transfer policy.

## Compiler–standard library boundary

The boundary remains sound. The required revision must keep execution-domain evidence generic and
must not make `Scheduler`, `Execution`, `Deferred`, or any standard-library actor compiler-known.

## Required revisions

1. Define `LocalExecution` as a same-thread execution-domain outcome with no execution-instance or
   Scheduler identity; cover parking and movement between same-domain frames while deferring the
   source Scheduler sufficiency proof to its later handoff slice.
2. Extend the ordinary-name collision scenario and task to `Deferred`, `Scheduler`, and one unrelated
   privileged-looking name.

## Next state

Revise the existing artifacts with `$openspec-update-change`, run strict validation, then run a fresh
`$slp-5-audit-openspec` pass. Do not implement from the o003 baseline.
