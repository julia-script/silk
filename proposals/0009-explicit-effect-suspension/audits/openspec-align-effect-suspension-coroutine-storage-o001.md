# OpenSpec audit o001: align-effect-suspension-coroutine-storage

SLP: `proposals/0009-explicit-effect-suspension/proposal.md`
SLP revision: 8
SLP digest: `fe98f333644496ce00caef5ba0046e99eaa0496ab69928c40e0a36b8b9affce6`
OpenSpec change: `align-effect-suspension-coroutine-storage`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `ec0accf47d569b8f60d01e96573f6813f1f2b7d5f135a60e3fc07159f84e74c8`
- `proposal.md`: `5951c9c95b3fae1bf5dd338aa4c7d7ed37075fa1809f3f5003be752ee06f9558`
- `design.md`: `8a06dced77b24965301eaf9aa71e2295fb27f51d2400e6b1a9693f3fb52b7666`
- `tasks.md`: `ec7e04313769f00bf1a50d3cf86f8666f1ba4c369ee0489d671896eaf28480e2`
- `specs/bootstrap-backend/spec.md`: `b144537f25ff30265d0f2d4933596cd82b6d8114a409863884bf1e4e7b2c3ed6`
- `specs/bootstrap-evaluation/spec.md`: `937f894fa1dc906706d34c6aae1f84a69383b22e94e3e5099dbb538719d72419`
- `specs/bootstrap-flow-functions/spec.md`: `611c2b7de08ee32add3418d543fc6e38895a3474629fbc180603a2d81ef3bbce`
- `specs/bootstrap-intrinsic-boundary/spec.md`: `1a31545ae09c86701f3a7ea263314b1790a931e37a70789e02bbc3ddb157904a`
- `specs/bootstrap-mir/spec.md`: `f75a17a4241316e085867502ed3cccbabb4673581ceba2d950fbcdd29309bc2e`
- `specs/bootstrap-owned-allocation/spec.md`: `2fe56f8e3d74be2dfd1928644a8bfeed0cf301ecc3b2fe20c415ba44d4fc2077`
- `specs/bootstrap-ownership/spec.md`: `04a3d3564a7c7d7571d3dff962a2e3704dcdff89ffb950a4d3e2891eba31b57e`

Date: 2026-08-19
Result: Ready

## Validation evidence

- `openspec status --change align-effect-suspension-coroutine-storage`: all four artifact groups
  complete.
- `openspec validate align-effect-suspension-coroutine-storage --strict`: valid.
- Manual bidirectional trace checked every SUSP-001–020 decision against normative scenarios,
  realization decisions, implementation tasks, and verification work.
- The audit added missing scenarios before freezing these digests: nested Effect success remains
  nested; every recursive cycle must cross suspension; unrelated and uncovered recursion remain
  valid; an ordinary allocator implementation may suspend; and evaluator-private storage
  exhaustion remains outside typed failure.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| SUSP-001 explicit source boundary | Flow: explicit lazy composition; intrinsic: one target-neutral primitive | D1, D5, D7 | 1.1, 2.1–2.2, 3.2, 6.1–6.2 | Covered |
| SUSP-002 ordinary child outcome; nested success stays nested | Flow: preserve nested Effect success | D1, D7 | 1.2, 7.1 | Covered |
| SUSP-003 every recursive cycle crosses suspension | Flow: cover mutual recursion with one suspension edge | D2, D4, D6 | 1.5, 5.5, 6.4–6.5 | Covered |
| SUSP-004 unrelated recursion receives no guarantee | Flow: ignore suspension on an unrelated branch | D2, D4 | 1.5, 6.4 | Covered |
| SUSP-005 exact `A ! E ? R` channels | Flow: preserve child channels; intrinsic: keep storage out of contract; MIR: reject storage channels | D1, D7 | 1.1, 2.1–2.4, 7.4 | Covered |
| SUSP-006 private execution-storage exhaustion is fatal | Flow/backend/evaluation exhaustion scenarios | D4, D6 | 1.5, 4.5, 6.6 | Covered |
| SUSP-007 child starts after complete transfer | Backend: parent transition before child; ownership: complete frame-state transition | D2, D3, D5 | 1.4, 3.4–3.5, 5.5, 6.4 | Covered |
| SUSP-008 ordinary combinators are transparent | Flow: runner seam, map, and flat-map scenarios | D1, D2, D7 | 1.2, 7.2 | Covered |
| SUSP-009 one reusable frame per invocation | Flow/backend/MIR repeated-suspension scenarios | D2–D4 | 1.3, 3.1–3.5, 4.5, 6.5 | Covered |
| SUSP-010 one static maximum frame shape | Flow and MIR maximum-layout requirements | D2–D4 | 1.3, 3.1, 3.3, 3.5, 4.5 | Covered |
| SUSP-011 ordinary ownership and loans continue | Ownership: owner, provider, and borrow scenarios | D3, D5, D6 | 1.4, 3.4–3.5, 4.4, 5.5, 6.4 | Covered |
| SUSP-012 transition has one complete owner state | Ownership: complete transition; MIR: missing owner rejected | D3, D5 | 1.4, 3.4–3.5, 5.5 | Covered |
| SUSP-013 exact structured cleanup | Ownership: deep success/failure and trap cleanup scenarios | D3, D5, D6 | 1.4, 4.5, 5.5, 6.4, 6.6 | Covered |
| SUSP-014 frame placement is private execution storage | Intrinsic: no storage contract; removed owned-allocation requirements; backend-private ABI | D4, D6 | 1.3, 2.3–2.5, 4.1–4.5, 7.4–7.5 | Covered |
| SUSP-015 no allocator implementation restriction | Flow: suspend inside allocator operation | D1, D4, D7 | 2.3, 2.5 | Covered |
| SUSP-016 suspended calls retain logical `CallDepth` | Evaluation: bound suspended logical recursion | D5 | 1.5, 5.4 | Covered |
| SUSP-017 evaluator/native/Wasm semantic parity | Evaluation, backend, and ownership parity requirements | D3–D6 | 5.5, 6.4, 9.1 | Covered |
| SUSP-018 non-suspending graphs pay no coroutine cost | Flow closed pipeline; backend native/Wasm artifact inspection | D7 | 7.3, 9.1 | Covered |
| SUSP-019 uncovered recursion stays valid; warning is LSP-only | Flow: leave an uncovered cycle valid | D2, D7 | 1.5, 8.2, 8.4 | Covered |
| SUSP-020 no async or scheduler semantics | Flow: do not interpret suspension as parking; intrinsic exclusions | D1, D7 and explicit non-goals | 7.3–7.4, 8.2, 9.1 | Covered |

## Completeness findings

### Missing normative behavior

None after revision. All programmer-observable SUSP-001–020 behavior has at least one normative
requirement and scenario.

### Missing boundary or failure scenarios

None after revision. Typed failure, fatal compiled and evaluator storage exhaustion, nested Effect
success, uncovered recursion, unrelated suspension, allocator independence, ownership, cleanup,
logical-depth limits, engine parity, and pay-for-use boundaries are explicit.

### Missing implementation or verification work

None. Every normative behavior maps to a concrete implementation or deletion task and a test,
structural audit, artifact inspection, or cross-engine verification task.

## Divergence findings

### OpenSpec contradictions or inventions

None. The change corrects the current allocator-backed implementation toward SLP-0009 rather than
turning implementation drift into language authority. It adds no parking, async, scheduler, fiber,
ordinary-recursion, or catchable-storage-failure behavior.

### SLP decisions requiring reconsideration

None. Private frame layout and execution-stack mechanics refine realization without changing the
accepted programmer model.

## Compiler–standard library boundary

The compiler receives one sealed, target-neutral suspension intrinsic and owns only transfer,
private execution state, target lowering, and fatal private-storage exhaustion. The standard
library exposes `Effect.suspend` as ordinary Silk with the exact `A ! E ? R` contract. The compiler
does not recognize the public actor or operation spelling, select a source allocator, add a service
requirement, synthesize a typed storage failure, or give ordinary combinators special suspension
knowledge. This satisfies minimal compiler privilege.

## Required revisions

None. The missing scenarios identified during review were incorporated before this audit was
frozen.

## Next state

The OpenSpec change is ready for implementation against SLP-0009 revision 8. Implementation must
not begin from a later SLP or modified OpenSpec artifact without a new audit digest and disposition.
