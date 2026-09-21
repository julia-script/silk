# Proposal

## Why

The revision query graph can validate complete checked units across adjacent in-memory snapshots,
but process restart discards both answers and ordered dependency observations. Persisting only TIR
bytes would not prove that current headers, imports, configuration, or selected dependencies still
agree, and the existing single-body codec trusts unbounded JSON and positional declaration IDs.

## What Changes

- Persist only complete `CheckBody` query records: the full checked unit, result fingerprint, exact
  descriptor, and ordered dependency manifest in one atomic Storage envelope.
- Load persisted candidates into the same shared query validator used for in-memory revision reuse;
  missing child records execute current providers rather than implying validity.
- Replace the trusting single-body TIR decoder with a bounded, versioned complete-unit codec that
  validates structure, tags, fields, references, ownership, provenance, and stable declarations.
- Re-resolve stable declarations against current headers and project admitted units through current
  presentation before exposing a loaded answer.
- Treat missing, stale, incompatible, corrupt, and oversize records as typed cache rejections that
  recompute; treat external Storage failures as explicit cache events that recompute on read or skip
  publication; propagate interruption and defects.
- Add explicit persistence configuration and fresh/cache-disabled bypass without persisting other
  semantic families.
- Correct the D2 roadmap text: fresh HIR and headers may be rebuilt before persisted checked-unit
  admission, so a HIR disk cache is not a correctness prerequisite.

## Capabilities

### New Capabilities

- `persisted-checked-units`: Selective Storage-backed persistence and strict admission for complete
  checked-unit semantic query records.

### Modified Capabilities

None.

## Impact

This changes semantic query snapshots and provider startup, complete checked-unit encoding and
presentation, compiler request/configuration plumbing, traces/reports, TIR codec tests, and focused
restart fixtures. It intentionally does not persist header answers, evaluation, instances, layouts,
MIR, or backend artifacts.
