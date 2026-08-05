# Design — establish-syntax-file-artifact

## Context

See proposal.md — Why. The spike's [Lexer.ts](../../../packages/compiler/src/Lexer.ts) and
[Parser.ts](../../../packages/compiler/src/Parser.ts) already preserve trivia, missing tokens, and
error regions, and their results are immutable — the losslessness is right. What is missing is the
pinned artifact boundary: one `SyntaxFile` per module (ticket 06's frontend paragraph), stable
element identities for later fact tables to key against, a distinct documentation-comment token
kind, and the first deterministic textual encoder with goldens.

## Goals / Non-Goals

**Goals**

- One immutable `SyntaxFile` per parsed module: source, token stream, surface tree, lexical and
  parser diagnostic collections.
- Deterministic element identities qualified by source identity, reproducible across runs.
- `///` documentation comments as a distinct trivia kind, no semantic attachment.
- Deterministic textual syntax encoder gated by committed golden files.
- Inspector reads the artifact directly, adding a trivia-inclusive token stream view.

**Non-Goals**

- No grammar changes beyond the doc-comment token kind (vertical slice stays frozen).
- No module loading — one source module per artifact until `load-module-closure`.
- No generic encoder abstraction (ticket 06 defers it until multiple formats exist).
- No binary format, no compatibility promise for encoder output.

## Decisions

1. **`Parser.parse` returns the `SyntaxFile`; `Lexer.lex` stays an intermediate.** The parser is
   where all artifact ingredients first coexist, so it bundles them; `LexicalResult` remains the
   lexer→parser handoff value and `ParseResult` is deleted. *Alternative rejected:* a separate
   `SyntaxFile.fromSource` orchestrator — it would import both phases from the module the parser
   itself imports, creating a cycle for one convenience call.

2. **Identities are derived, not stored on elements.** `SyntaxFile` computes a pre-order walk at
   construction and answers identity lookups from it; identity is `{sourceId, ordinal}` where the
   ordinal is the element's pre-order position. Tokens and nodes stay exactly as they are —
   `SyntaxTree.make` and `Token.make` signatures do not change, and pre-order over a deterministic
   tree is deterministic. Foreign elements answer `None`. *Alternative rejected:* storing an id
   field on every node/token — threads an allocator through every construction site for no
   behavioral gain.

3. **Doc comments lex inside the existing comment branch.** A comment whose first three bytes are
   `///` becomes `DocComment`; everything else stays `LineComment`. Both remain trivia to the
   parser. `////…` is a `DocComment` by the exactly-`///`-prefix rule reading (first three bytes),
   matching rustdoc's tolerance without a fourth kind.

4. **Encoder is a plain line-based text format.** Header (source identity, byte count), then the
   token stream (ordinal, kind, span, escaped slice), the tree (indented kinds with spans, missing
   and error entries named explicitly), then both diagnostic collections. Non-printable bytes are
   escaped as `\xNN` so invalid UTF-8 encodes losslessly. No version marker — ticket 06 attaches
   no compatibility promise to encoder output.

5. **Goldens are committed text files compared byte-for-byte.** One accepted fixture and one
   malformed fixture (missing tokens + error region + invalid bytes). Updating a golden is a
   deliberate reviewed diff, which is the gate ticket 06 asks for.

6. **`SemanticAnalysis.Result.parse` becomes `Result.syntax`.** Downstream field paths
   (`parse.lexical.source` → `syntax.source`, `parse.diagnostics` → `syntax.parserDiagnostics`)
   migrate mechanically; the evaluator never touched parse fields and is unaffected.

## Risks / Trade-offs

- [Every parse-result consumer breaks at once] → The rename is mechanical and type-driven; the
  compiler package, inspector, and release-candidate consumer script are the complete consumer
  set, all in-repo.
- [Reference-keyed identity map relies on tree sharing] → The tree is built once per artifact and
  frozen; tokens are shared by reference from the stream into the tree, which the existing
  token-traversal test already guarantees.
- [Golden files drift from format tweaks] → Goldens live beside fixtures and fail byte-exactly;
  the format is deliberately boring to keep diffs readable.

## Migration Plan

1. Land `SyntaxFile.ts` (artifact type, identity walk, encoder) with the parser returning it.
2. Re-point semantic analysis, compiler tests, inspector, and the release-candidate consumer
   script; delete `ParseResult`.
3. Add doc-comment lexing and the golden tests last — both are additive once the artifact exists.
4. Rollback is git-revert; no persisted data.

## Open Questions

None — canonical module identity and multi-module loading stay with `load-module-closure`.
