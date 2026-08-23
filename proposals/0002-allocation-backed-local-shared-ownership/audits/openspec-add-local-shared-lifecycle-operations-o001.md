# OpenSpec audit o001: add-local-shared-lifecycle-operations

SLP: `proposals/0002-allocation-backed-local-shared-ownership/proposal.md`
SLP revision: 6
SLP digest: `c97959718e551d9d4c4273e6503a18630696c6ac969087192bc3e5133c4ca069`
OpenSpec change: `add-local-shared-lifecycle-operations`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `b9399e03673ba9cf76a3aaed652b32793349b0b8bda5bafea77ccfa94a12a811`
- `proposal.md`: `680854d933fb4653fe073cc8dfd8b7dfe34068238a2fc7997f75b19b144ffd3c`
- `design.md`: `435e6bbc6b0217db11aa730be0a8837d2dd975262a0a529ca393c6228ed4d74d`
- `specs/bootstrap-intrinsic-boundary/spec.md`: `e4acf6dfe87f8d05bceec7fd11b756323e415e71b89821544e054fa322c0a809`
- `specs/bootstrap-ownership/spec.md`: `72989118cacb08fb4bb290966d034725bdb87fa77a8e096f1816b4a24884866f`
- `tasks.md`: `086109f3ddab50da98f879645b2dab0cf7e889dde16ce64b300b925621fa1df5`

Canonical spec baselines:

- `openspec/specs/bootstrap-intrinsic-boundary/spec.md`: `201a6ae4f28b556bbec4fa098d678a9d2b1ca7fd023bab45204bc9e860d75224`
- `openspec/specs/bootstrap-ownership/spec.md`: `57bc933cc255f9238bc6e1e5adeddf5cdcb7e1533bf639ff63476502bce3eec6`

Date: 2026-08-22
Result: Ready

## Validation evidence

- `openspec status --change add-local-shared-lifecycle-operations --json` reported complete planning.
- `openspec instructions apply --change add-local-shared-lifecycle-operations --json` reported state
  `ready`, twelve tasks total, zero complete.
- `openspec validate add-local-shared-lifecycle-operations --strict --json --no-interactive`
  passed after one bounded OpenSpec fix pass: one valid change, zero issues.
- Three fresh reviewers read the accepted SLP, raw change, and canonical intrinsic/ownership specs.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| Clone is allocation-free, payload-inert, and traps before count overflow mutation | Clone below limit; trap before mutation | Independent target-planned count transition | 1.2 transition, allocation, and payload probes | Covered |
| Access is one allocation-free exclusive callback region | Select callback; clean unselected callback | `Available | Active`, one restricted loan, ordinary callable cleanup | 1.3, 2.1 state, cleanup, and sequential tests | Covered |
| Every nested public access shape conflicts without a second reference | Conflict every nested combination | One all-exclusive primitive | 4.1 semantic and transition matrix | Covered |
| Callback borrows cannot escape or suspend | Direct, recursive-container, and suspension scenarios | Recursive position-restricted-borrow checking | 2.2–2.4 stable code and two-span diagnostics | Covered |
| Strong count and access state are independent | Clone during access; two-handle drop | Separate machines | 1.3, 3.2 state-transition tests | Covered |
| Last drop alone cleans `T` then releases allocation | Drop order; typed-failure frames; cycle leak | One opaque-core cleanup action | 3.1–3.3 ownership plans and fixtures | Covered |
| Traps promise no unwind cleanup | Qualified acyclic cleanup requirement | No restoration/cleanup after fatal trap | 3.3 and canonical trap semantics | Covered |
| The compiler surface remains the minimum two lifecycle calls | Lifecycle inventory | No exposed lanes, authorities, conflict value, or actor names | 1.1 exhaustive inventory and same-spelling tests | Covered |

## Completeness findings

### Missing normative behavior

- Access allocation freedom was omitted. The fix pass makes `sharedWithMut` allocation-free and adds
  explicit verification. Closed.
- Cleanup of the unselected affine take-once callback was unspecified. The fix pass requires ordinary
  exactly-once cleanup after normal selected-callback return, with successful access restored first.
  Closed.
- The unconditional acyclic-cleanup sentence contradicted no-unwind trap semantics. It now applies to
  handles discharged through structured execution and explicitly excludes fatal traps. Closed.

### Missing boundary or failure scenarios

- Recursive escape and suspension paths lacked a single stable diagnostic identity and complete
  access-boundary/escape provenance. The requirements and tasks now require both. Closed.
- Lifecycle inventory tests omitted state/count/address/authority/conflict exposure and same-spelling
  declarations. The fix pass covers the complete prohibited surface. Closed.

### Missing implementation or verification work

- Clone's allocation-free and payload-inert guarantees lacked probes; task 1.2 now requires them.
- Task 3.2 incorrectly forbade a legal `2 -> 1` non-receiver drop during access. It now verifies that
  the borrowed receiver may become the sole obligation but cannot itself be consumed or trigger final
  cleanup while borrowed. Closed.

## Divergence findings

### OpenSpec contradictions or inventions

One proposed High finding requested a cross-target common numeric strong-count maximum. Rejected:
SLP-0002 selects a target-bounded representation, and parity compares evaluation with the backend for
the same selected target. The design and scenario now make that normalization explicit; no source
failure channel or different accepted behavior was introduced.

### SLP decisions requiring reconsideration

None.

## Compiler–standard library boundary

Ready. The compiler owns only count transition, one callback-shaped access transition, restricted
loan checking, and opaque-core cleanup. Public inspection, mutation, conflict/trap policy, and actor
names remain ordinary source concerns.

## Required revisions

Completed in one fix pass: allocation-free access, callback cleanup, target-selected maximum
normalization, no-unwind qualification, complete diagnostic provenance, exhaustive privilege gates,
clone probes, and the corrected legal non-receiver drop assertion.

## Next state

Ready. Implement after the semantic and allocation prerequisites, then pass the SLP-5 gates before
the standard-library slice.
