# Design — unify-compiler-diagnostics

## Context

See proposal.md — Why. The spike has three structurally similar but independent diagnostic types
([LexicalDiagnostic.ts](../../../packages/compiler/src/LexicalDiagnostic.ts),
[ParseDiagnostic.ts](../../../packages/compiler/src/ParseDiagnostic.ts),
[SemanticDiagnostic.ts](../../../packages/compiler/src/SemanticDiagnostic.ts)). Each phase result
already exposes diagnostics as ordered readonly data with source-owned spans — the behavior is
right; the shape is triplicated and has no phase identity, no causal links, and no cross-phase
ordering authority. Existing stable codes (e.g. `SEM0003`) are load-bearing in specs and tests.

## Goals / Non-Goals

**Goals**

- One `Diagnostic` concept module in the compiler package; phases share the shape, keep their
  own collections.
- Deterministic diagnostic identity so sentinels and causal links survive re-runs bit-for-bit.
- One driver-side merge/sort; nothing else ever orders across phases.

**Non-Goals**

- No renderer work beyond the inspector panel (human/machine renderers arrive with the driver
  proposals).
- No new diagnostic content: existing codes, messages, and spans migrate as they are.
- No canonical *module* identity yet — that arrives with `load-module-closure`; the sort key
  degrades gracefully to (span, code, tie-breaker) while compilation is single-file.

## Decisions

1. **One concept module, per-phase collections.** `Diagnostic.ts` owns the model; phase results
   keep exposing their own readonly collections (the modified specs preserve this). Merging is a
   pure function the driver owns. *Alternative rejected:* a single accumulated stream threaded
   through phases — couples phase signatures together and breaks the immutable-result-per-phase
   discipline before the snapshot exists.

2. **Originating phase is a closed literal union** (`'lexical' | 'parser' | 'semantic'`, growing
   with later phases), not a free string — exhaustive switches in the inspector and driver stay
   honest.

3. **Diagnostic identity is deterministic data, not object reference**: derived from originating
   phase, stable code, primary span, and an ordinal among equals within its phase result. Since
   every phase is already deterministic, identity is reproducible across runs — which is what
   makes causal links encodable and golden-testable. *Alternative rejected:* incrementing global
   IDs — deterministic only within one process wiring, and they leak allocation order into
   artifacts.

4. **Sentinels extend the existing unavailability idiom.** The spike's `Unavailable`/`Absent`
   variants gain an optional originating diagnostic identity. No new sentinel type — the idiom
   the whole realignment preserves is exactly this one, now with provenance.

5. **Structured reasons stay, and known related spans surface.** The parser's and semantic
   phase's typed `reason` payloads are part of the unified model (per-code structured data), not
   flattened into messages — the inspector and tests depend on them. The duplicate-name reasons'
   `originalSpan` additionally surfaces as a labeled related span, so the unified model's
   related-spans field is exercised from day one instead of shipping unused. *Alternative
   rejected:* message-only diagnostics — loses machine-readable reason data the specs already
   promise.

6. **Machine-applicable edits are modeled but unpopulated.** The field exists (ticket 06 pins
   it); no current diagnostic emits one. Populating edits is per-diagnostic future work, not part
   of unification.

## Risks / Trade-offs

- [Shape churn touches every fixture] → Migrate codes/messages/spans verbatim; only the wrapper
  shape changes, so fixture diffs stay mechanical and reviewable.
- [Causal links invite retroactive rewrites of existing diagnostics] → Only wire causes where a
  sentinel already exists (unresolved references, unavailable contracts); no hunting for new
  cascade relationships in this change.
- [Driver doesn't exist yet] → The merge/sort is a pure exported function with its own tests;
  today's only consumer is the inspector panel. The real driver adopts it unchanged in
  `accept-end-to-end-pipeline`.

## Migration Plan

1. Land `Diagnostic.ts` alongside the existing types; migrate lexer → parser → semantic in order,
   deleting each old type as its phase migrates.
2. Update the inspector last, against the merged stream.
3. Rollback is git-revert; no persisted data or external consumers exist.

## Open Questions

None — remaining unknowns (canonical module identity in the sort key, edit population) are
explicitly deferred to their owning proposals without affecting these specs or tasks.
