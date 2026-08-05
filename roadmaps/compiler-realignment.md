# Compiler realignment — spike → pinned pipeline

> **Status: complete (2026-08-05).** All 13 proposals are implemented and archived. The spine
> runs end to end — compilation request → closure → HIR → ownership → instances → MIR → LLVM
> bitcode → pinned Clang object → linked native executable — with the differential harness and
> byte-identical determinism gates running continuously and every phase visualizable in its lab.
> **The grammar freeze is lifted**: language features are now ordinary proposals that flow
> through every phase and appear in every lab.

Move `packages/compiler` from the spike shape (lexer → parser → semantic-analysis monolith →
tree-walking evaluator, single file) to the pipeline pinned in
[issue 06](../wayfinder/bootstrap-language/issues/06-bootstrap-compiler-pipeline.md).
Nothing in the spike is sacred; the fact-table idiom, lossless syntax, and the inspector
projection pattern survive.

**Strategy: vertical slice first.** The grammar slice stayed frozen while the spine was driven
down to a native executable. Proposal 13 landed; grammar widening is unfrozen.

## Definition of done (every phase proposal)

A phase is complete only when:

1. it publishes immutable facts reachable through the analysis facade,
2. it has a deterministic textual encoder with golden tests (ticket 06), and
3. the inspector renders a projection of it — every step produces visual, human feedback.

Rule that enforces 3 forever: **the inspector consumes only the facade, never phase internals.**

## Foundation vs. features

This plan is foundation-only: no user-visible language surface is added except where the
pipeline's own shape requires it. Exactly two language-visible additions are in scope:

- **Import declarations** (proposal 3) — closure loading needs syntactic imports; the current
  single-file grammar has none. Minimal spelling, issue 08 owns the final form.
- **Minimal bindings/moves** (proposal 7) — **resolved: stay frozen.** The vacuous check still
  lands the phase, fact table, cleanup-plan artifact, encoder, facade query, and lab; bindings
  arrive with issue 08's syntax work.

Everything else (HIR, MIR, ownership, instances, backend, linker) changes what the compiler
*is*, not what the language *has*. Feature work builds on this foundation afterwards.

## Proposals

### Track 1 — SyntaxFile alignment

1. **Unified diagnostics model** *(first — everything publishes into it)*
   - One `Diagnostic` shape: stable code, severity, primary + related spans, originating phase, causal ID
   - Migrate `LexicalDiagnostic` / `ParseDiagnostic` / `SemanticDiagnostic`; deterministic sort in one place (driver-side)
   - Discards: the three per-phase diagnostic shapes

2. **SyntaxFile artifact**
   - Bytes + trivia tokens + surface tree under one per-file identity; stable IDs on every node and token
   - First textual encoder + golden tests
   - Inspector: syntax lab reads `SyntaxFile` — tokens and tree, missing/error nodes highlighted
   - Discards: loose `Lexer.Result` / `Parser.ParseResult` shapes

### Track 2 — split the monolith

3. **Module closure loading**
   - Compilation request → root + syntactic imports to full closure; canonical module identities; module-level cycle reporting (issue 04)
   - Adds minimal import syntax (see Foundation vs. features)
   - Inspector: import graph, cycles marked

4. **Declaration headers phase**
   - Canonical decl IDs before any body; public signatures; header fact table
   - Inspector: declaration index table

5. **HIR + body elaboration**
   - Split `SemanticAnalysis`: integrated name/type elaboration constructing HIR; fact tables keyed by canonical IDs; HIR encoder + goldens
   - Inspector: HIR view, type hover over source
   - Discards: the `SemanticAnalysis` monolith (fact idiom survives)

6. **Analysis facade**
   - Formal query surface over the snapshot; `flow-model` and all labs migrate onto it
   - May fold into 5 if thin — kept separate so the facade-only rule isn't an afterthought

### Track 3 — ownership

7. **Ownership check + cleanup plan (slice-sized)**
   - Checker over generic HIR for the frozen slice — trivially satisfiable is fine; the phase, its fact table, and the cleanup-plan artifact exist from day one
   - Inspector: scope/lifetime timeline over source spans
   - Resolved: stay frozen; bindings/moves arrive with issue 08's syntax work

### Track 4 — instances + MIR

8. **MIR definition + encoder**
   - Ops, basic-block CFG, logical Silk types, provenance; encoder + goldens; no lowering yet (hand-built samples)
   - Open: MIR inspector view lands here (against samples) or in 9?

9. **Instance discovery + HIR→MIR lowering**
   - Deterministic worklist, instance keys (decl ID + normalized args — degenerate without generics, shape is real); lowering inserts drops/cleanup edges from the ownership proof
   - Inspector: CFG view, cleanup edges highlighted, hover → source provenance

10. **MIR interpreter**
    - Re-target the evaluator: analysis facts → MIR; trace events tied to MIR ops; Evaluated layer rewires; differential harness scaffold (old-vs-new evaluator as migration check)
    - Kept deliberately: semantics oracle for differential testing, forces MIR meaning independent of LLVM, only source for the inspector's dynamic layer. Severable leaf if maintenance ever outweighs value.
    - Discards: `BootstrapEvaluation` in its current position

### Track 5 — backend + link

11. **Backend service + bitcode emission**
    - `Backend` service contract; `LlvmBackend` over `packages/llvm`; explicit target-layout input; deterministic bitcode

12. **Toolchain orchestration**
    - Pinned `clang -c`, build-scope-owned intermediates, `NativeLinker`/`ClangLinker` (invokes the Clang driver — no linker of our own), minimal runtime shim slice (issue 07)
    - Inspector: artifact/provenance lab (commands, sizes, outputs)

13. **End-to-end acceptance**
    - Driver wiring source → executable; differential testing interpreter-vs-native across the corpus; byte-identical determinism checks; phase timing counters
    - The spine is declared done here; grammar widening unfreezes

## Dependencies

Mostly linear (1 → 2 → 3 → 4 → 5 → 7 → 8 → 9 → 10 → 11 → 12 → 13); 6 lands with or after 5;
8 may proceed in parallel with 7.

## Status

Proposals are written as openspec changes:

- [x] 1 — [unify-compiler-diagnostics](../openspec/changes/archive/2026-08-05-unify-compiler-diagnostics/proposal.md)
- [x] 2 — [establish-syntax-file-artifact](../openspec/changes/archive/2026-08-05-establish-syntax-file-artifact/proposal.md)
- [x] 3 — [load-module-closure](../openspec/changes/archive/2026-08-05-load-module-closure/proposal.md)
- [x] 4 — [collect-declaration-headers](../openspec/changes/archive/2026-08-05-collect-declaration-headers/proposal.md)
- [x] 5 — [elaborate-bodies-to-hir](../openspec/changes/archive/2026-08-05-elaborate-bodies-to-hir/proposal.md)
- [x] 6 — [establish-analysis-facade](../openspec/changes/archive/2026-08-05-establish-analysis-facade/proposal.md)
- [x] 7 — [check-ownership-and-cleanup](../openspec/changes/archive/2026-08-05-check-ownership-and-cleanup/proposal.md)
- [x] 8 — [define-mir-and-encoder](../openspec/changes/archive/2026-08-05-define-mir-and-encoder/proposal.md)
- [x] 9 — [discover-instances-and-lower-to-mir](../openspec/changes/archive/2026-08-05-discover-instances-and-lower-to-mir/proposal.md)
- [x] 10 — [retarget-evaluator-to-mir](../openspec/changes/archive/2026-08-05-retarget-evaluator-to-mir/proposal.md)
- [x] 11 — [establish-backend-service](../openspec/changes/archive/2026-08-05-establish-backend-service/proposal.md)
- [x] 12 — [orchestrate-native-toolchain](../openspec/changes/archive/2026-08-05-orchestrate-native-toolchain/proposal.md)
- [x] 13 — [accept-end-to-end-pipeline](../openspec/changes/archive/2026-08-05-accept-end-to-end-pipeline/proposal.md)
