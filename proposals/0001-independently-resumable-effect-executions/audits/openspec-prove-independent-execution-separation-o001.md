# OpenSpec audit o001: prove-independent-execution-separation

SLP: `proposals/0001-independently-resumable-effect-executions/proposal.md`
SLP revision: 31
SLP digest: `963a7420f16bce3bd0ec50acd906b4a4ee43319d132c5452d3686cde643c5635`
OpenSpec change: `prove-independent-execution-separation`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `b9399e03673ba9cf76a3aaed652b32793349b0b8bda5bafea77ccfa94a12a811`
- `proposal.md`: `ca4209206a5d09f186205d3e9ae8faa42e0e38f37ee75db9950d9d1142c49cfc`
- `design.md`: `27bc6634b9e287baa018ea417b41d50f367c0f8edcbf988f23414a8408abdade`
- `specs/bootstrap-independent-execution-pressure/spec.md`: `9562b454a8585d55da307c8a74a513bf37957c1ed552e995036e4c5eb7348bc3`
- `specs/bootstrap-language-pressure-programs/spec.md`: `b542003190fc521b6f4c3d0093e30650f3c237f2f11b41a7bd2ff075f0dbaac0`
- `specs/bootstrap-backend/spec.md`: `01fcf0c3b20fba1e1e2e31f20a8c15a303d024369b961b1d70911c20d2a44f51`
- `tasks.md`: `19d7bf3d568f6054ac6a719e789b16a1c63fed153726ec8828e87551d3a152cd`

Canonical spec baselines:

- `openspec/specs/bootstrap-language-pressure-programs/spec.md`: `87cd080f811134269f19318a76d7a4c21d3d7f38e1bc7f7a96cff83d161220dc`
- `openspec/specs/bootstrap-backend/spec.md`: `f31429aab77dc9b437c0fc804e934e88a669c07002e5f01f6d9e4be88e555e19`

Date: 2026-08-22
Result: Ready

## Validation evidence

- `openspec validate prove-independent-execution-separation --strict --json --no-interactive`:
  passed after repair pass 1, one valid change, zero issues.
- The three fresh lenses reported the bulk of the cross-change traceability findings here: canonical
  backend pay-for-use wording, pre/post-publication failure separation, homogeneous erasure evidence,
  explicit nested-only coverage, multi-dormant no-scan evidence, closed-leaf/provider boundaries,
  timer service/failure ordering, alternate-owner dormant destroy, documentation, and findings-
  artifact scope. All were OpenSpec revisions; none required an SLP decision.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| Distinct exact bodies become homogeneous `Execution<TaskOutput>` before owner publication | Homogeneous first-activation scenario | Scheduler-shaped source witness | 1.1–1.2 storage/activation tests | Covered |
| Deferred wait/producer progresses through push readiness | Source-result and many-dormant scenarios | Shared Deferred/ReadyInbox, extract then wake | 1.3, 1.7 and 2.3 | Covered |
| Timer owns provider, prepares failure before park, and adds no Scheduler/Fiber row | Timer success/failure scenarios | Explicit outer driver and local reactor | 1.4–1.5 | Covered |
| Alternate owner reuses substrate and cleans drop-while-yielded | Coroutine completion/drop scenarios | Shared port owner | 1.6 cross-engine cleanup | Covered |
| Source owner remains acyclic and child is closed leaf | Acyclic endpoint and nested-provider rejection | ReadyInbox-only endpoint topology | 1.1, 1.8 | Covered |
| Publication admission differs from later waiter failure | Pre-publication and post-publication failure scenarios | Reservation rollback and later waiter path | 2.1–2.2 failure sweeps | Covered |
| Full static matrix includes explicit nested-only ownership | Five pay-for-use configurations and modified canonical backend requirement | Five minimal programs and runtime inventories | 3.1–3.2 structural evidence | Covered |
| No actor-name privilege and deferred decisions stay deferred | Rename/audit/findings scenarios | Inventory plus renamed fixtures | 3.3–3.5 | Covered |
| User model and retained-Wake cost are documented | Documentation requirement | Pressure actors remain non-canonical | 3.6 documentation verification | Covered |
| Smaller alternative remains rejected without compatibility path | Three distinguishing scenarios | Evidence report, no fallback | 3.5 findings report | Covered |

## Completeness findings

### Missing normative behavior

None after repair pass 1.

### Missing boundary or failure scenarios

None. The final plan now includes homogeneous erasure, multiple dormant tasks, waiter failure after
publication, timer preparation failure, nested-provider rejection, cancellation, alternate-owner
drop, unowned entry diagnostics, and all five static configurations.

### Missing implementation or verification work

None. Every added scenario maps to a source fixture, structural artifact check, differential test,
failure sweep, findings report, or user-documentation task.

## Divergence findings

### OpenSpec contradictions or inventions

None after modifying the canonical backend pay-for-use requirement to exclude explicit Execution
construction from its former universal no-suspension condition.

### SLP decisions requiring reconsideration

None.

## Compiler–standard library boundary

Ready. All owners, queues, results, waiters, timers, reactors, payloads, rollback guards, and policy
remain ordinary Silk over the general substrate and SLP-0002 Shared. Renaming and inventory gates
forbid source-name privilege; implicit roots, canonical concurrency/Coroutine APIs, and parallel
transfer remain deferred.

## Required revisions

Repair pass 1 completed for every reviewer finding listed in Validation evidence. No open revisions
remain, and the second permitted repair pass was not needed.

## Next state

Implementation-ready after `add-independent-execution-engine-parity` and the SLP-0002 dependency
chain; proceed through `$slp-5-implement`.
