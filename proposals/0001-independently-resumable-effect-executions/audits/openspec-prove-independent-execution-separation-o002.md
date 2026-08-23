# OpenSpec audit o002: prove-independent-execution-separation

SLP: `proposals/0001-independently-resumable-effect-executions/proposal.md`
SLP revision: 31
SLP digest: `963a7420f16bce3bd0ec50acd906b4a4ee43319d132c5452d3686cde643c5635`
OpenSpec change: `prove-independent-execution-separation`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `b9399e03673ba9cf76a3aaed652b32793349b0b8bda5bafea77ccfa94a12a811`
- `proposal.md`: `ca4209206a5d09f186205d3e9ae8faa42e0e38f37ee75db9950d9d1142c49cfc`
- `design.md`: `757a9dbe4bbda9fc26d3796779038c05e398a85c4c84688837751365e63276da`
- `specs/bootstrap-backend/spec.md`: `01fcf0c3b20fba1e1e2e31f20a8c15a303d024369b961b1d70911c20d2a44f51`
- `specs/bootstrap-independent-execution-pressure/spec.md`: `7ace46f4e34843250e1901d98b46175c46cb347ce833440bef24fde6aeb5f033`
- `specs/bootstrap-language-pressure-programs/spec.md`: `b542003190fc521b6f4c3d0093e30650f3c237f2f11b41a7bd2ff075f0dbaac0`
- `tasks.md`: `c9211487a8415ef1ef927301b1119e810e1ad243f2fd4d9db5d535ed2cd9e075`

Canonical spec baselines:

- `openspec/specs/bootstrap-backend/spec.md`: `7b6fd0eae33a4743baad87a32bf432a095936284df447e3d2d080c43d370f9a6`
- `openspec/specs/bootstrap-language-pressure-programs/spec.md`: `f825289369c2837679bb8ddc70af235d3f5478bae309a9a95289fce8516ce756`

Date: 2026-08-23
Result: Ready

## Validation evidence

- Planning freeze reported complete artifacts, 21 pending tasks, and implementation readiness.
- `openspec validate prove-independent-execution-separation --strict --json --no-interactive`:
  one valid change, zero issues after repair pass 2.
- Three independent lenses checked all SLP examples and negative evidence against the landed SLP-0002
  witness and test harness. The plan now consumes that evidence as a companion rather than
  duplicating or weakening it.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| Scheduler-shaped owner stores homogeneous executions | Activation scenarios | Shared TaskStore/ReadyInbox companion | 1.1–1.2 | Covered |
| Deferred, timer, and Coroutine shapes share substrate | Connected witness scenarios | Ordinary source actors | 1.3–1.8 | Covered |
| Failure admission and later waiter failure differ | Failure scenarios | Extend landed quota harness | 2.1–2.2 | Covered |
| Shared access ends before wake/callback | Notification scenarios | Extraction then signal | 2.3 | Covered |
| Affinity does not select execution machinery | Shared direct/nested pay-for-use scenarios | Separate local-Shared runtime slice | 3.1–3.2 | Covered |
| Actor names confer no privilege | Rename and inventory scenarios | Reuse landed normalization | 3.3–3.5 | Covered |
| Five execution tiers pay only for use | Structural matrix | Companion fixtures and corpus | 3.1–4.2 | Covered |

## Completeness findings

### Missing normative behavior

None. `LocalExecution` affinity is now explicitly separated from package/Wake selection.

### Missing boundary or failure scenarios

None. The witness covers publication rollback, waiter failure, stale identities, cancellation,
alternate-owner drop, unowned entry, and Shared-affine direct/nested variants.

### Missing implementation or verification work

None. The tasks reuse the landed canonical/renamed fixtures, one-analysis-per-source discipline,
allocation projections, quota sweeps, and designated native corpus.

## Divergence findings

### OpenSpec contradictions or inventions

None. The SLP-0002 witness remains independently runnable, and no source actor becomes canonical.

### SLP decisions requiring reconsideration

None.

## Compiler–standard library boundary

Ready. Scheduler, Deferred, Timer, Coroutine, queues, payloads, rollback, and policy remain ordinary
Silk over general Execution/Wake and Shared substrate. Implicit roots and parallel transfer remain
deferred.

## Required revisions

Repair pass 1 converted the witness to a companion of landed SLP-0002 evidence. Repair pass 2 added
Shared-affine pay-for-use controls. No revisions remain.

## Next state

Implementation-ready after `add-independent-execution-engine-parity`; the SLP-0002 dependency is
already archived and satisfied.
