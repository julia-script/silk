# OpenSpec audit o001: add-independent-execution-packaging

SLP: `proposals/0001-independently-resumable-effect-executions/proposal.md`
SLP revision: 31
SLP digest: `963a7420f16bce3bd0ec50acd906b4a4ee43319d132c5452d3686cde643c5635`
OpenSpec change: `add-independent-execution-packaging`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `b9399e03673ba9cf76a3aaed652b32793349b0b8bda5bafea77ccfa94a12a811`
- `proposal.md`: `142014b54210a602a96cee1f28d87a88ca860efe76f93d1924680f6a2c3d01de`
- `design.md`: `1284eae901763b89485cbd837a341cc726db1d5e88ea962dd6a71bb9a83221fe`
- `specs/bootstrap-independent-execution-packaging/spec.md`: `a96f91799b9b633086a493b5e11505051e97d37b667a8af31b7750d2d295d462`
- `specs/bootstrap-owned-allocation/spec.md`: `4412f6f4a7a6df48995b981909e9ac9e78b6858b61568611b89967a8598898d0`
- `specs/bootstrap-target-layout/spec.md`: `652606ca7176d49ee2500fd645f43a59b179475e13403ece76b9ed75b9587940`
- `specs/bootstrap-intrinsic-boundary/spec.md`: `3b7c600fef297ce40a8da96384a8f041d9ac1fbdfdf978e83083b83837c93ba3`
- `specs/bootstrap-ownership/spec.md`: `2d56d4f7fac821d29a8b6e7fdba49ad2cb48266e62ccc216938011a07789ce91`
- `tasks.md`: `4b3f62d21b4ee121347ebcf155480973e5d950244e2591f98cf4a54b2f2fc5a4`

Canonical spec baselines:

- `openspec/specs/bootstrap-owned-allocation/spec.md`: `00bfee8a21aa1008b0a346cc72a25a547a170e4ac6800c572fdf4848ccbfc881`
- `openspec/specs/bootstrap-target-layout/spec.md`: `f51eb88256bf3730dae7dd462e09326d5fd5de43e71200a1e66ed74854497d69`
- `openspec/specs/bootstrap-intrinsic-boundary/spec.md`: `201a6ae4f28b556bbec4fa098d678a9d2b1ca7fd023bab45204bc9e860d75224`
- `openspec/specs/bootstrap-ownership/spec.md`: `57bc933cc255f9238bc6e1e5adeddf5cdcb7e1533bf639ff63476502bce3eec6`

Date: 2026-08-22
Result: Ready

## Validation evidence

- `openspec validate add-independent-execution-packaging --strict --json --no-interactive`:
  passed after the cross-change repair pass, one valid change, zero issues.
- Three fresh reviewers found no packaging-local omission or invention. Cross-change findings about
  explicit nested-only pay-for-use and canonical backend wording were repaired in the final evidence
  slice without changing this package contract.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| One exact caller-funded combined package | Construction, provenance, refusal, single-owner scenarios | Package plan keyed by A/F/O/R/target/summary | 1.1–2.4 layout/provenance/failure tests | Covered |
| Fixed endpoint with zero-sized non-parking path | Zero-sized endpoint and layout configurations | O plus reusable R(&O) | 1.2, 2.2, 2.4 structural tests | Covered |
| Callback drive transfers one affine branch | Completion, suspension, nested-transfer scenarios | Consuming MIR terminal edges | 3.1–3.3 ownership/callback tests | Covered |
| Execution-local logical roots | Alternating-owner depth scenario | Saved per-owner context | 3.4 logical-depth evidence | Covered |
| Exact cleanup and fatal later growth | Never-driven, completion, failure-data, exhaustion scenarios | Cleanup metadata plus existing fatal stack policy | 3.5–3.6 cleanup/trap tests | Covered |

## Completeness findings

### Missing normative behavior

None.

### Missing boundary or failure scenarios

None. Allocation refusal, provenance mismatch, illegal drive state, never-driven drop, typed failure
reification, and fatal post-construction growth are explicit.

### Missing implementation or verification work

None.

## Divergence findings

### OpenSpec contradictions or inventions

None.

### SLP decisions requiring reconsideration

None.

## Compiler–standard library boundary

Ready. Only Layout, unsafe from-Allocation initialization, and safe callback-shaped drive are added.
Allocator policy, safe wrappers, error rows, scheduling actors, step sums, explicit destroy, and
implicit roots remain outside the compiler.

## Required revisions

None local to this change.

## Next state

Implementation-ready after `establish-independent-execution-semantics`.
