# OpenSpec audit o002: add-external-wake-parking

SLP: `proposals/0001-independently-resumable-effect-executions/proposal.md`
SLP revision: 31
SLP digest: `963a7420f16bce3bd0ec50acd906b4a4ee43319d132c5452d3686cde643c5635`
OpenSpec change: `add-external-wake-parking`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `b9399e03673ba9cf76a3aaed652b32793349b0b8bda5bafea77ccfa94a12a811`
- `proposal.md`: `c85b7bcd6b00cf889a03b709e26288a2bfe06f252455e7b46705cfde018c1696`
- `design.md`: `e8e9471be8abd39155a75688025e028c685eecbc749e727dfb0cc0918ef1d1d7`
- `specs/bootstrap-external-wake-parking/spec.md`: `bda153fe0f5bf024a9b60cd58c1023f890fc21326c98ff4dd02dfc55c52fe732`
- `specs/bootstrap-intrinsic-boundary/spec.md`: `09a13dd4d1949f1681cf99e6606c659968e43d222bffdbf1ab568a0a1f65bcd3`
- `specs/bootstrap-ownership/spec.md`: `fe1fd5434de155693926f5a4ac420d73e07f7c5843bf108b5a51293480a0e4c6`
- `specs/bootstrap-semantic-facts/spec.md`: `ea8492d78a48a7ade5bbaf0c5fcdfae2adc69c2d0a7ca37f43bcb3941ddd5e22`
- `tasks.md`: `1f4126ea758980577c451c18fea2c7842095b2b10016fd47d181c59d24a44a71`

Canonical spec baselines:

- `openspec/specs/bootstrap-intrinsic-boundary/spec.md`: `1c3360e4eb0b8a9e2ec85b41f0ca22e65a8171e8380cac5aab7a7095bdc9b47f`
- `openspec/specs/bootstrap-ownership/spec.md`: `eeb6163007fa687f20938dd327b9fc208b6d73c0e1634e45471a7df0109e5beb`
- `openspec/specs/bootstrap-semantic-facts/spec.md`: `34150e7d06f9404c349b6d6f0243a59bfeeaef475d718da8a54edf525fa1b27b`

Date: 2026-08-23
Result: Ready

## Validation evidence

- Planning freeze reported complete artifacts, 16 pending tasks, and implementation readiness.
- `openspec validate add-external-wake-parking --strict --json --no-interactive`: one valid change,
  zero issues after repair pass 2.
- Three independent lenses verified the post-SLP-0002 affinity, Shared-access, ownership, and
  callable-mode seams. All findings were repaired; no SLP decision was required.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| Register-before-suspend and wake-before-dormant | Park ordering scenarios | Registering/Latched handoff | 2.1–2.4 | Covered |
| Wake is affine, local, and payload-free | Wake plus semantic-affinity requirements | Stable generation cell, canonical lattice | 1.1–1.3, 3.6 | Covered |
| Registration consumes affine captures | NonParking `once fn` contract | One registration transition | 1.1, 2.1 | Covered |
| Shared handle may park; Shared access may not | Cross-change ownership scenarios | Existing local-Shared seam | 3.6, 4.1 | Covered |
| DestroyPending defers reentrant cleanup | Notification scenarios | Invocation retain | 3.1–3.3 | Covered |
| Late Wake retains only inert package authority | Cancellation/reclaim scenarios | Authority accounting | 3.4–3.5 | Covered |
| Transfer policy remains deferred | Affinity-without-consumer scenario | Local facts, no atomics | 3.6, 4.2 | Covered |

## Completeness findings

### Missing normative behavior

None. Sealed Wake now normatively seeds canonical `LocalExecution` and propagates through source
state and suspended frames.

### Missing boundary or failure scenarios

None. Affine registration captures, active Shared-access rejection, latched destruction, late Wake,
reentrant destruction, generation reuse, and deferred transfer consumption are explicit.

### Missing implementation or verification work

None. `SuspensionOwnership` extension and all repository gates are assigned.

## Divergence findings

### OpenSpec contradictions or inventions

None. The plan no longer conflates affine ownership with execution affinity and does not invent a
worker-transfer operation or diagnostic.

### SLP decisions requiring reconsideration

None.

## Compiler–standard library boundary

Ready. Wake, wake, and park are the only new powers. Payloads, queues, timers, scheduling, unlink
policy, allocation, explicit cancellation, thread transfer, and atomics remain source-owned/deferred.

## Required revisions

Repair pass 2 added normative Wake affinity, `once fn` registration, Shared-access seam coverage,
canonical suspension ownership, precise affine wording, and mandatory gates. No revisions remain.

## Next state

Implementation-ready after `add-independent-execution-packaging`.
