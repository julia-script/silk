# Silk Language Proposals

Silk Language Proposals (SLPs) decide conceptual language and standard-library direction before
implementation work enters OpenSpec.

```text
SLP (direction and coherence)
  -> OpenSpec change(s) (normative deltas and implementation tasks)
    -> canonical specs and docs (implemented language truth)
```

- [Process](PROCESS.md) — lifecycle, evidence, bounded review, and OpenSpec boundary.
- [Template](TEMPLATE.md) — required proposal metadata and sections.
- [Review template](REVIEW-TEMPLATE.md) — durable record for one fixed-revision review round.
- [Ledger template](LEDGER-TEMPLATE.md) — finding ledger shared across review rounds.
- [OpenSpec audit template](OPENSPEC-AUDIT-TEMPLATE.md) — planning coverage and alignment record.
- [Implementation audit template](IMPLEMENTATION-AUDIT-TEMPLATE.md) — post-implementation conformance record.

## Proposals

- [SLP-0001: Independently resumable Effect executions](0001-independently-resumable-effect-executions/proposal.md)
  — Accepted direction; scheduler-neutral independently resumable execution and race-free parking
  for ordinary-source owners without selecting the canonical Fiber API.
- [SLP-0002: Allocation-backed local shared ownership](0002-allocation-backed-local-shared-ownership/proposal.md)
  — Accepted direction; explicit single-threaded shared lifetime with scoped interior access and
  last-handle cleanup, without atomics or compiler-known stateful actors.
- [SLP-0003: Implicit ownership for park-capable Effect entries](0003-implicit-ownership-for-park-capable-effect-entries/proposal.md)
  — Draft; split from SLP-0001 to decide synchronous root execution ownership, storage, target
  validation, and final-outcome adaptation while preserving ordinary `run` as the desired API.
