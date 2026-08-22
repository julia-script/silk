# OpenSpec audit o001: add-independent-execution-engine-parity

SLP: `proposals/0001-independently-resumable-effect-executions/proposal.md`
SLP revision: 31
SLP digest: `963a7420f16bce3bd0ec50acd906b4a4ee43319d132c5452d3686cde643c5635`
OpenSpec change: `add-independent-execution-engine-parity`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `b9399e03673ba9cf76a3aaed652b32793349b0b8bda5bafea77ccfa94a12a811`
- `proposal.md`: `e871fda8995a87656d76ad27ef2769be490546e42afd83a991313b0de79acc07`
- `design.md`: `896be546c7a64c15035c2ead537e297d6101b144c1e48bdca40d14f502144e4e`
- `specs/bootstrap-independent-execution-engine-parity/spec.md`: `144ab93da5eb92dfc463f957ca8cce32af49ba41947ae0c253d7417be0af402d`
- `specs/bootstrap-evaluation/spec.md`: `6f8c9b00222c5eb9c283dc5ef8eff9e220b07d1c4c2705ec013f2472ab584d51`
- `specs/bootstrap-backend/spec.md`: `2a9dad4ce8e9e06bbff841487463624c8debef9c81947b6dad8f7172e7e47936`
- `tasks.md`: `97f9f5a4da758dacab67d0db152d2193cd02ae9fa26244c28c61668c754ecd7a`

Canonical spec baselines:

- `openspec/specs/bootstrap-evaluation/spec.md`: `35c61d8e9b53b91e64eec8d3cc428db47aab8ca4a8bae2a9b2ae72f74bff1632`
- `openspec/specs/bootstrap-backend/spec.md`: `f31429aab77dc9b437c0fc804e934e88a669c07002e5f01f6d9e4be88e555e19`

Date: 2026-08-22
Result: Ready

## Validation evidence

- `openspec validate add-independent-execution-engine-parity --strict --json --no-interactive`:
  passed after repair pass 1, one valid change, zero issues.
- Fresh reviewers identified missing repeated backend-emission evidence and the need to exercise an
  explicit nested-only Execution configuration. Backend determinism is now an explicit task; the
  final pressure slice normatively and operationally covers explicit nested-only execution.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| All lifecycle/wake transitions are verified before lowering | MIR requirement and negative scenarios | MIR as authority boundary | 1.1–1.3 positive/negative/deterministic tests | Covered |
| Each execution owns a logical root | Alternating non-LIFO scenario | Explicit evaluator records | 2.1–2.3 oracle/trace tests | Covered |
| Evaluator/native/Wasm agree on ordering and cleanup | Owner-selected behavior scenarios | Shared continuation foundation per owner | 3.1–4.2 differential evidence | Covered |
| Local reactor delivery adds no thread/atomic policy | Reactor requirement and scenarios | Local state with abstract authority | 3.3, 4.3 | Covered |
| Backend artifacts are deterministic | Backend determinism scenario | Verified plan plus target-private helpers | 3.5 repeat-emission evidence | Covered |
| Nested/LIFO remains cheap and fatal traps remain no-unwind | Backend and parity trap scenarios | Reused nested foundation | 3.4, 4.2 | Covered |

## Completeness findings

### Missing normative behavior

None after the cross-change explicit-nested configuration repair.

### Missing boundary or failure scenarios

None. Invalid MIR, illegal drive, wake-order branches, typed data, stack exhaustion, target reactor
unavailability, and no-atomic local behavior are explicit.

### Missing implementation or verification work

None after adding repeated native/Wasm emission evidence.

## Divergence findings

### OpenSpec contradictions or inventions

None.

### SLP decisions requiring reconsideration

None.

## Compiler–standard library boundary

Ready. Engines lower only verified general execution/wake operations. Physical states, helpers,
layouts, growth, and ABI remain private; Scheduler, Timer, Fiber, Deferred, Coroutine, root policy,
and cross-thread execution remain absent.

## Required revisions

Repair pass 1 completed: backend determinism task and cross-change explicit-nested pay-for-use case.
No open revisions remain.

## Next state

Implementation-ready after `add-external-wake-parking`.
