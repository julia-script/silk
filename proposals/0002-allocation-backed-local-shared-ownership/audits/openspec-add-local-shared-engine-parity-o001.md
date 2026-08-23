# OpenSpec audit o001: add-local-shared-engine-parity

SLP: `proposals/0002-allocation-backed-local-shared-ownership/proposal.md`
SLP revision: 6
SLP digest: `c97959718e551d9d4c4273e6503a18630696c6ac969087192bc3e5133c4ca069`
OpenSpec change: `add-local-shared-engine-parity`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `b9399e03673ba9cf76a3aaed652b32793349b0b8bda5bafea77ccfa94a12a811`
- `proposal.md`: `216d001952bfefd2d96afe6d64969ceab506ffd6521110da491d4824d9c3724e`
- `design.md`: `dd157b522867dd736227075326b7bb74a4a766bf8c16a7818e8e50220576c17d`
- `specs/bootstrap-mir/spec.md`: `cd7c17ea999d16dec6f82465e8288a61a825252012f2e3607fb030559ef8270f`
- `specs/bootstrap-evaluation/spec.md`: `d58d3c2ed314b4bd19a573f4f775b89e4d13eb06dba289b68cc55bbbb863a9b8`
- `specs/bootstrap-backend/spec.md`: `82c3b2520ad641dbafc9281e920896c4a02bb332bb666b675507cd35a6b92538`
- `tasks.md`: `37d6a13881660950fa918112f847757b22dff80a3fd56ddeeb4c2af4784ac61f`

Canonical spec baselines:

- `openspec/specs/bootstrap-mir/spec.md`: `84158c69d0c951fdbf99a835c74c0ef51b36a500ce55c155a5b6a7433c965fbb`
- `openspec/specs/bootstrap-evaluation/spec.md`: `35c61d8e9b53b91e64eec8d3cc428db47aab8ca4a8bae2a9b2ae72f74bff1632`
- `openspec/specs/bootstrap-backend/spec.md`: `f31429aab77dc9b437c0fc804e934e88a669c07002e5f01f6d9e4be88e555e19`

Date: 2026-08-22
Result: Ready

## Validation evidence

- Status reported complete planning; apply instructions reported state `ready` with twelve tasks and
  zero complete before implementation.
- Strict validation passed after one bounded fix pass: one valid change, zero issues.
- Three fresh reviewers read the accepted SLP, raw change, and canonical MIR, evaluation, and backend
  specs. The realization lens mapped every original scenario to a design and task with no orphan work
  or test-tier violation.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| All engines execute one verified target-neutral lifecycle | Verified local-shared MIR; complete lifecycle scenario | Explicit MIR operations and verifier | 1.1–1.2 | Covered |
| Evaluation is the logical oracle | Sequential access, conflict, count/drop, cleanup, cycle, overflow | Logical block identity and bounded events | 2.1–2.3 | Covered |
| Native and Wasm agree on observable transitions | Success, four conflicts, overflow, typed failure, representation privacy, cycle | Target-local non-atomic storage | 3.1–3.3, 4.1–4.3 | Covered |
| Clone/access allocate nothing and add no synchronization/runtime policy | Allocation-free backend scenario | Direct target-local state operations | 3.3 structural/runtime probes | Covered |
| Malformed provenance, callbacks, ownership, or access loans never reach execution | Three MIR rejection scenarios | One pre-engine verifier | 1.1 diagnostics and no-partial-artifact tests | Covered |
| Count checks precede mutation and last cleanup precedes release | Evaluator/backend overflow and cleanup scenarios | Compare/trap dominance and canonical cleanup call | 2.2, 3.1–3.3, 4.3 | Covered |
| Tests use the cheapest falsifying tier | All scenarios | Evaluator oracle, Wasm physical reclaim, native corpus boundaries | 2.3, 4.1–4.4 | Covered |

## Completeness findings

### Missing normative behavior

- Evaluator overflow previously required only a pre-mutation check. It now requires the fatal
  no-unwind trap, no wrap/saturation/store, and no partial handle. Closed.
- Backend cost policy previously forbade only allocator channels. It now forbids private allocation
  or reallocation, locks, atomics, scheduler machinery, GC backing, background work, and source-name
  runtime dispatch, with structural/runtime evidence. Closed.

### Missing boundary or failure scenarios

MIR rejection existed only in design and tasks. The fix pass added the `bootstrap-mir` capability
delta covering mismatched provenance, reused/unconsumed inputs, unavailable types, malformed callback
contracts, escaping access state, stable diagnostics, and no engine/partial artifact entry. Closed.

### Missing implementation or verification work

No remaining gap. Tasks already covered the evaluator, both backends, differential corpus, reduced
overflow maximum, Wasm reclamation, deterministic MIR, and boundary-only native failure ordinals;
they now additionally pin verifier diagnostics and zero-cost clone/access probes.

## Divergence findings

### OpenSpec contradictions or inventions

None. Adding the MIR delta makes an existing declared impact and task normative; it does not widen
compiler privilege or introduce a source-visible operation.

### SLP decisions requiring reconsideration

None.

## Compiler–standard library boundary

Ready. MIR and engines know only canonical intrinsic operations and verified ownership/layout facts.
They do not know `Shared`, Deferred, Scheduler, ready-inbox, allocator policy, or conflict policy by
spelling, and clone/access add no hidden runtime subsystem.

## Required revisions

Completed in one fix pass: normative MIR verification, fatal evaluator overflow, strict backend cost
boundary, and matching diagnostic/structural/runtime verification tasks.

## Next state

Ready after the first four SLP-0002 slices. Implement evaluator, MIR, native, and Wasm parity before
the final sufficiency witness.
