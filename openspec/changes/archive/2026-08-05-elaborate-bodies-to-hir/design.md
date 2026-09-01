# Design — elaborate-bodies-to-hir

## Context

See proposal.md — Why. After `collect-declaration-headers`, the monolith's remaining half is body
analysis over concrete syntax. Ticket 06's step 3 makes resolution, typing, contract validation,
and HIR construction one integrated phase; its IR paragraph pins HIR as resolved, typed, and
canonically identified with source provenance. The fact idiom survives; the monolith file does
not.

## Goals / Non-Goals

**Goals**

- `Elaboration.ts` replaces `SemanticAnalysis.ts`: same fact shapes, same diagnostics, plus HIR
  construction and normalized contracts, consuming collected headers.
- `Hir.ts`: typed core operations (integer literal, parameter reference, call), canonical call
  targets, explicit unavailable states with causes, normalized contracts, deterministic textual
  encoder with goldens.
- Evaluator and inspector flow model re-point mechanically; the inspector gains an HIR panel with
  type-and-span hover.

**Non-Goals**

- No new typing power: the frozen slice has one type, so "generic-aware" and "contract rows"
  reduce to their degenerate forms — the structure (normalized contract as data) is what lands.
- No cross-module call resolution: import _bindings_ do not exist yet (issue 04's binding rules
  arrive with the language surface); resolution stays module-local.
- No MIR, no evaluator semantics changes — `retarget-evaluator-to-mir` owns the evaluator's
  future.

## Decisions

1. **`Elaboration.ts` carries the fact shapes forward under their existing names.**
   `ExpressionFact`, `FunctionFact`, `Result`, and friends move from `SemanticAnalysis.ts`
   verbatim; `elaborateModule(syntax)` replaces `analyze(syntax)` with the same
   result contract, so the evaluator, flow model, and inspector migrate by import swap.
   `SemanticAnalysis.ts` is deleted — no compat facade; the release-candidate surface swaps
   `./SemanticAnalysis` for `./Elaboration` and `./Hir`.

2. **HIR is projected inside the elaboration phase from the typed facts.** One phase, two
   internal steps: fact analysis (existing logic), then HIR projection per function. The phase
   boundary — headers in, facts + HIR + diagnostics out — is what ticket 06 pins; internal
   staging is an implementation detail. _Alternative rejected:_ constructing HIR inline during
   fact analysis — interleaves two concerns the facts already separate cleanly.

3. **Dependency-graph walking is the trivial order in this slice.** All contracts are explicit
   (declared types), so no on-demand memoization or SCC contract requirement can fire;
   declarations elaborate in header order. The ticket's structure is honored by contracts being
   _inputs_ to body elaboration (from the header phase), never inferred from bodies.

4. **HIR expressions carry `type`, `span`, and — for unavailable states — the originating
   diagnostic identity** (from the existing caused sentinels). Calls reference canonical
   declaration identities; a call can only resolve to a first-present-occurrence declaration, so
   a resolved target always has a canonical identity.

5. **Contracts normalize from headers**: every parameter type and the return type resolved →
   `Contract {parameters, result}`; otherwise `Unavailable` with the first unresolved type's
   cause. An unavailable contract never appears as an empty valid one.

6. **Encoder format mirrors the syntax encoder's conventions**: line-based, indented, spans in
   `[start, end)`, one `hir-module` header, `fn` lines with canonical identity (or
   `duplicate`/`unidentified` markers) and contract, expression lines with kind, resolved type,
   and span. Goldens: one accepted multi-function fixture, one damaged fixture exercising
   unavailable states.

## Risks / Trade-offs

- [Deleting `SemanticAnalysis` breaks any unnoticed consumer] → The compiler package, docs app,
  and release-candidate script are the complete consumer set; TypeScript finds the rest.
- [Fixture churn] → Test files swap `SemanticAnalysis.` for `Elaboration.`; assertions are
  otherwise unchanged because fact shapes and diagnostics are unchanged.
- [HIR projection could drift from facts] → It is a pure function of the facts with golden and
  determinism tests; drift shows up as a golden diff.

## Migration Plan

1. Rename/move: `SemanticAnalysis.ts` → `Elaboration.ts` (`analyze` → `elaborateModule`), keep
   all fact logic; update compiler tests.
2. Add `Hir.ts` (types, projection, contract normalization, encoder) and wire into the
   elaboration result; goldens + tests.
3. Re-point evaluator, flow model, inspector; add the HIR panel with hover.
4. Update exports, release-candidate surface; delete `SemanticAnalysis.ts`.
5. Rollback is git-revert.

## Open Questions

None — evaluator-on-MIR, ownership, and cross-module binding belong to later proposals.
