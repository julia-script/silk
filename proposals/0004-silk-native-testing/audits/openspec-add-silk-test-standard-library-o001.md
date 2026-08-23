# OpenSpec audit o001: add-silk-test-standard-library

SLP: `proposals/0004-silk-native-testing/proposal.md`
SLP revision: 35
SLP digest: `a23040d98a7c4028e759f000bc586ebbc539366dbca75a987886753eaf627205`
OpenSpec change: `add-silk-test-standard-library`
Schema: `spec-driven`
Artifact digests:

- `.openspec.yaml`: `cef97c66dca8a4822d16808d0c827486f612c60b5fa247f48cdf6eb25f768e94`
- `proposal.md`: `99974d1f15705fbebeee7941e678cf05c535affe8555b7f15853a3d868649a61`
- `design.md`: `cfef9f874c0b2d52efba50ec3bc7c5739cd9c9eb3985ee70f567522691b24cda`
- `specs/bootstrap-silk-stdlib/spec.md`: `add3e6ccc7f7d9524b967b64e37a6e09c79a5e19f3d31588f20b769587c57370`
- `tasks.md`: `2d9b40b3e7301a0f0fe644abd6a2752af8105544442648967a818bed81e1019c`

Canonical spec baseline:

- `openspec/specs/bootstrap-silk-stdlib/spec.md`: `26465e7b27e2b490393e393d2b40802a2d5dc72b23d49664ace0ee418dad35eb`

Date: 2026-08-23
Result: SLP decision required

## Validation evidence

- `openspec status --change add-silk-test-standard-library --json` reported complete planning.
- `openspec instructions apply --change add-silk-test-standard-library --json` reported state
  `ready`, twelve tasks total, zero complete.
- Strict OpenSpec validation passed structurally: one valid change, zero issues.
- Three fresh reviewers read the accepted SLP, prerequisite inventory change, raw stdlib change,
  and canonical stdlib spec. Fidelity found one SLP decision conflict, so the handoff stopped before
  editorial repair or downstream audits as required by the SLP process.

## Direction-to-plan traceability

| SLP decision, invariant, or example | Requirement and scenario | Design realization | Task and verification | Disposition |
| --- | --- | --- | --- | --- |
| Ordinary-source Test actor over sealed primitives | Canonical actor and renamed wrapper | Thin safe wrappers | 1.1–1.3, 4.1 | Covered |
| Silent boolean and byte-slice assertions | Initial assertions | Ordinary Failure branch and byte loop | 2.1–2.2 | SLP naming decision required |
| Public completed-case Events and mutable Reporter | Reporter requirement | Outcome then Event then lexical report | 2.3 | Coverage revision also required |
| Raw ASCII-folded OR filters | Standard filtering | Bytewise source matcher | 3.1 | Scenario correction required |
| Deterministic standard runner and statuses 0/1/2 | Standard runner | Per-case reporter and edge status mapping | 3.2–3.3 | Precedence/edge revision required |
| Complete path, presentation-only filtering | Presentation requirement | Source reporter over logical frames | 3.4 | Source display boundary decision absent |

## Completeness findings

### Missing normative behavior

- The runner edge lacks a normative scenario for skipping argument zero, reading owned filter bytes
  in order under ordinary OsHostInput/Allocator provision, and cleaning them on acquisition failure.
- Public StackFrame fields, borrowed accessors, checked out-of-range behavior, literal pattern-like
  filter bytes, PASS/FAIL lines, summary output, and output failure lack exact scenarios.
- The no-match scenario incorrectly says any individual OR-filter miss returns 2; only an empty
  aggregate selection does.
- Mid-suite ReportError precedence and stopping behavior are not explicit.

### Missing boundary or failure scenarios

The real Outcome-to-Event-to-Reporter path does not yet prove exactly-once StackPath reclamation on
reporter success and failure, or owned filter/format/output cleanup on input, allocation, output,
no-match, and report-failure exits.

### Missing implementation or verification work

The design/tasks assume a source-visible logical-to-display path resolver and a closed
StandardReporter dependency graph, but neither change defines how ordinary Silk receives that
mapping, owns or borrows its lifetime, translates output/allocation failures to ReportError, and
retains no residual requirement besides Reporter at `Test.report`.

## Divergence findings

### OpenSpec contradictions or inventions

The no-match scenario contradicts OR selection. The unconditional exactly-once suite language also
conflicts with the accepted early exit on infrastructure failure; status 2 must override accumulated
case status and later cases must remain uninvoked. These are OpenSpec repairs after resolution.

### SLP decisions requiring reconsideration

The accepted SLP fixes the shipped typed assertion error as `Test.Failure`. The canonical
`bootstrap-silk-stdlib` requirement mandates descriptive PascalCase shipped error declarations
ending in `Error`, without aliases. OpenSpec cannot rename the accepted API or violate the canonical
contract. The author must resolve the direction; the audit recommends `Test.FailureError`.

## Compiler–standard library boundary

Not yet closed. Reporting, matching, and presentation remain correctly assigned to ordinary source,
but the display-path mapping and StandardReporter dependency closure require a privilege-free source
mechanism after the SLP naming decision is resolved.

## Required revisions

First route the `Failure` versus `FailureError` fork through SLP resolution. If the SLP returns to an
accepted direction, revise the standard-library artifacts for aggregate no-match semantics, exact
runner-edge ownership, StackFrame accessors, literal filters, presentation output, infrastructure
precedence, Event/path cleanup, a source-visible display mapping, and a closed Reporter provider graph.

## Next state

SLP decision required. Invoke the manual `slp-3-resolve` step; do not audit
`add-silk-test-command` or `prove-silk-native-testing-sufficiency` until the accepted direction and
its frozen digest are updated.

## Subsequent author resolution

Revision 36 resolves this audit's SLP-level naming fork as `Test.AssertionError`. The author rejected
the audit's `Test.FailureError` recommendation as awkward; `AssertionError` precisely names the
ordinary error emitted by `Test.assert` and `Test.equalBytes`, while `Outcome.Failed` remains the
broader result of any unhandled typed test failure. This note does not change the frozen o001 result
or artifact digests; the OpenSpec change requires revision and a fresh handoff audit.
