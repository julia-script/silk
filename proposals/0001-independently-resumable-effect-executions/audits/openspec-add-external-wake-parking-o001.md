# OpenSpec audit o001: add-external-wake-parking

SLP: `proposals/0001-independently-resumable-effect-executions/proposal.md`
SLP revision: 31
SLP digest: `963a7420f16bce3bd0ec50acd906b4a4ee43319d132c5452d3686cde643c5635`
OpenSpec change: `add-external-wake-parking`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `b9399e03673ba9cf76a3aaed652b32793349b0b8bda5bafea77ccfa94a12a811`
- `proposal.md`: `1ddb528a7dd2bdcb8e924c5d666c194c61e3ac1ef3c8474b251ef6654ea2c338`
- `design.md`: `e8e9471be8abd39155a75688025e028c685eecbc749e727dfb0cc0918ef1d1d7`
- `specs/bootstrap-external-wake-parking/spec.md`: `c0c659619bd1ac11de23d576395eb019b2e073221492fd50d88180bf77a18084`
- `specs/bootstrap-intrinsic-boundary/spec.md`: `eb56c7eb78a5efd9216cb59335604d55edd207b4cd3f791cf58b4827c59ee418`
- `specs/bootstrap-ownership/spec.md`: `fe1fd5434de155693926f5a4ac420d73e07f7c5843bf108b5a51293480a0e4c6`
- `tasks.md`: `7c50aa5d2c743bab5cbe928e8c0b7b5f2a7015b9ebd9245791009d2a4eef65ee`

Canonical spec baselines:

- `openspec/specs/bootstrap-intrinsic-boundary/spec.md`: `201a6ae4f28b556bbec4fa098d678a9d2b1ca7fd023bab45204bc9e860d75224`
- `openspec/specs/bootstrap-ownership/spec.md`: `57bc933cc255f9238bc6e1e5adeddf5cdcb7e1533bf639ff63476502bce3eec6`

Date: 2026-08-22
Result: Ready

## Validation evidence

- `openspec validate add-external-wake-parking --strict --json --no-interactive`: passed after
  repair pass 1, one valid change, zero issues.
- Fresh fidelity and completeness reviewers identified a canonical “exactly one suspension” conflict
  and incomplete generation-reuse authority ordering. The delta now modifies the canonical nested-
  suspension requirement explicitly and forbids reuse until Wake and every transient/invocation
  authority ends.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| Register-before-suspend and wake-before-dormant | Park registration and four ordering scenarios | Registering/Latched handoff gate | 2.1–2.4 ordering tests | Covered |
| Fixed-layout affine Wake carries readiness only | Wake local/payload-free scenarios | One generation capability over stable cell | 1.1–1.3, 3.6, 4.1 | Covered |
| Notifying prevents drive; DestroyPending defers cleanup | Notification scenarios | End mutation, invocation retain, deferred destroy | 3.1–3.3 reentrancy tests | Covered |
| Late cancelled Wake is a safe no-op retaining whole package | Cancellation/reclaim scenarios | Abstract authority count excluding G | 3.4 cleanup/reclaim tests | Covered |
| Cell reuse waits for every prior authority | Generation reuse scenario | Generation reset after Wake/transients/invocation | 3.5 validation tests | Covered |
| Nested suspension remains one separate primitive | Modified canonical suspension requirement | Distinct nested versus external modes | 4.2 inventory audit | Covered |

## Completeness findings

### Missing normative behavior

None after repair pass 1.

### Missing boundary or failure scenarios

None. Latched destroy, late Wake, eligible drop, reentrant drive/destroy, forgotten Wake, repeated
generation, local extraction, and worker-thread rejection are explicit.

### Missing implementation or verification work

None.

## Divergence findings

### OpenSpec contradictions or inventions

None after modifying the canonical nested-suspension requirement rather than silently contradicting
its exact-one wording.

### SLP decisions requiring reconsideration

None.

## Compiler–standard library boundary

Ready. Wake, consuming wake, and callback-shaped park are the only added powers. Payloads, queues,
timers, unlink policy, scheduling, allocation, cancellation API, and cross-thread atomics remain in
ordinary source or later proposals.

## Required revisions

Repair pass 1 completed: canonical suspension wording and complete generation-reuse authority gate.
No open revisions remain.

## Next state

Implementation-ready after `add-independent-execution-packaging`.
