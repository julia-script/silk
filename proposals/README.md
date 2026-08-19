# Silk Language Proposals

Silk Language Proposals (SLPs) decide conceptual language and standard-library direction before
implementation work enters OpenSpec.

```text
SLP (direction and coherence)
  -> OpenSpec change(s) (normative deltas and implementation tasks)
    -> canonical specs and docs (implemented language truth)
```

- [Process](PROCESS.md) — lifecycle, evidence, review, convergence, and OpenSpec boundary.
- [Template](TEMPLATE.md) — required proposal metadata and sections.
- [Review template](REVIEW-TEMPLATE.md) — durable record for one fixed-revision review round.
- [OpenSpec audit template](OPENSPEC-AUDIT-TEMPLATE.md) — planning coverage and alignment record.
- [Implementation audit template](IMPLEMENTATION-AUDIT-TEMPLATE.md) — post-implementation conformance record.

## Proposals

- [SLP-0001: Module-level static value composition](0001-static-value-composition/proposal.md)
  — Draft; parked until static schema composition becomes a stabilization priority.
- [SLP-0002: Explicit non-unit result discard](0002-explicit-result-discard/proposal.md)
  — Draft; records the selected strict explicit-discard model and awaits Candidate review.
- [SLP-0003: Canonical value types and narrow compatibility](0003-canonical-value-types/proposal.md)
  — Draft; workbench for the foundational values-and-types stabilization pass.
