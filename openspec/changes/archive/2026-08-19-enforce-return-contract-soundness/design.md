## Context

Return validation is fragmented across declaration completion, expression elaboration, conformance mapping, and MIR construction. Some mismatches therefore become an unavailable fact too late or are carried as invalid executable structure.

## Goals / Non-Goals

**Goals:** establish one semantic return proof for every body; keep invalid bodies out of executable HIR/MIR; diagnose the source boundary once.

**Non-goals:** implicit `run`, automatic Effect flattening, return coercions, or backend recovery from invalid MIR.

## Decisions

1. Declaration completion records a resolved return contract before any body can become executable.
2. Body analysis returns a proof covering every reachable explicit return and fallthrough path.
3. Mapped conformance targets use the same declaration proof and specialized contract check before
   witness publication. The later inline-conformance implementation must publish inline bodies
   through that same declaration seam rather than adding a second return checker.
4. Reachability treats a declaration lacking the proof as unavailable and preserves the semantic diagnostic as primary.
5. MIR builders accept only proven bodies and retain an invariant check for hand-built/compiler-bug inputs.

## Risks / Trade-offs

- Earlier rejection may expose previously hidden errors in repository fixtures; every fixture must migrate rather than receive a compatibility path.
- Recursive declarations require provisional signatures but never provisional body validity.

## Migration Plan

Add the shared proof and diagnostics, migrate ordinary bodies, then conformance bodies, gate HIR/MIR, update issue 226 regression coverage, and delete downstream invalid-return handling.

## Open Questions

None at the language level. Diagnostic codes and internal proof representation are implementation details constrained by existing catalogs.
