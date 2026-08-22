# OpenSpec audit o001: prove-local-shared-slp1-sufficiency

SLP: `proposals/0002-allocation-backed-local-shared-ownership/proposal.md`
SLP revision: 6
SLP digest: `c97959718e551d9d4c4273e6503a18630696c6ac969087192bc3e5133c4ca069`
OpenSpec change: `prove-local-shared-slp1-sufficiency`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `b9399e03673ba9cf76a3aaed652b32793349b0b8bda5bafea77ccfa94a12a811`
- `proposal.md`: `da1daadfb1746cc4ae64c609566876703cf8bd7fe1951966458ab165f1d73c6a`
- `design.md`: `442cd83ed83650f753e8bff8c27694b1ac6e5fbd95c2533eb234852060e0863e`
- `specs/bootstrap-language-pressure-programs/spec.md`: `3417308caa06d17e4fcbf548c8ba2471d5abb04a4b9046a897b41860fb1e8c9d`
- `tasks.md`: `dd9de3e6a4fb90271101c1e647aeea2c0531738a9b2ce3388760b2de4f4f1af8`

Canonical spec baseline:

- `openspec/specs/bootstrap-language-pressure-programs/spec.md`: `87cd080f811134269f19318a76d7a4c21d3d7f38e1bc7f7a96cff83d161220dc`

Date: 2026-08-22
Result: Ready

## Validation evidence

- Status reported complete planning; apply instructions reported state `ready` with ten tasks and
  zero complete before implementation.
- Strict validation passed after one bounded fix pass: one valid change, zero issues.
- Three fresh reviewers read the accepted SLP, raw change, and canonical pressure-program spec.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| Several dormant computations share one ready inbox without escaping `&mut` | Enqueue and dormant Effect scenarios | Fixed-capacity inbox with explicit cloned handles | 1.1–1.2 | Covered |
| Producer and waiters share one Deferred-style state/value | Publish-after-extract scenario | Store affine value in shared state; extract only callbacks | 1.3, 2.1 | Covered |
| External callbacks run only after access restoration | Publish scenario | Extract-then-invoke | 1.3, 2.1 | Covered |
| Unpublished affine payloads and dormant captures clean exactly once | Last-drop and dormant Effect scenarios | Ordinary core and Effect capture cleanup | 1.2, 2.2 | Covered |
| Construction failure produces no partial actor and reruns remain deterministic | Construction-failure scenario | Ordinal sweep and rollback | 2.3 | Covered |
| Evaluation, native, and Wasm agree | Engine scenario | Differential corpus and tiered failure ordinals | 2.3, 3.1, 4.1 | Covered |
| No witness or public actor becomes compiler-known | Rename scenario | Name-independent artifacts and inventory | 3.2 | Covered |
| Findings separate removed shared-state wall from SLP-0001 execution work | Boundary and complete-report scenarios | Canonical pressure findings contract | 3.3 | Covered |

## Completeness findings

### Missing normative behavior

- The design incorrectly moved the published affine value out with callbacks. It now stores the
  offered value once in shared state, extracts only callbacks, and lets all waiter handles observe the
  same source-owned value through shared borrows. Closed.
- A callable-only witness could have skipped Effect retention. The delta and tasks now require a
  stored callable and dormant unrun Effects, including early dropped-Effect cleanup. Closed.
- Construction failure was task-only. A normative ordinal-sweep scenario now pins typed failure, no
  partial actor, constructor-input cleanup, balanced release, and deterministic recovery. Closed.

### Missing boundary or failure scenarios

The phrase “one-time publication” risked inventing unspecified repeated-publication policy. The
change now scopes itself to one witness publication call and verifies one transition/value owner and
one callback per waiter; repeated-publication API policy remains explicitly out of scope. Closed.

### Missing implementation or verification work

The findings task did not satisfy the canonical pressure-report contract. It now requires category,
evidence, disposition, smallest follow-up, comparison with lexer/stack-VM evidence, and evidence-led
next work in addition to the SLP-0001 boundary. Closed.

## Divergence findings

### OpenSpec contradictions or inventions

The affine-value extraction contradiction is repaired. No repeated-publication error/result model or
public Deferred API was invented.

### SLP decisions requiring reconsideration

None.

## Compiler–standard library boundary

Ready. Ready inbox, Deferred-style state, Scheduler-like names, callbacks, and witness wrappers are
ordinary pressure source. The compiler and engines consume only the general SharedCore, ownership,
callable/Effect, MIR, and backend contracts established by prior slices.

## Required revisions

Completed in one fix pass: correct shared payload retention, explicit Effect pressure, normative
construction rollback, scoped single-publication evidence, and the full findings-report gate.

## Next state

Ready after the standard-library and engine-parity slices. Implement as evidence only; do not begin
SLP-0001 execution transfer, parking, or public Scheduler/Deferred APIs.
