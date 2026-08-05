## Why

The spike's lexer and parser already preserve trivia and represent missing and error nodes, but
their outputs are loose per-phase result shapes without stable identities. The pinned pipeline
names one artifact — the lossless `SyntaxFile` — as the frontend's foundation, with stable IDs
that every later fact table keys against, and a deterministic textual encoder gating byte-identical
output.

## What Changes

- **BREAKING**: Bundle original source bytes, the trivia-preserving token stream, and the
  source-faithful surface tree into one immutable `SyntaxFile` artifact per loaded source module,
  replacing the loose `Lexer` and `Parser` result shapes.
- Assign stable source IDs and byte spans to every tree node and token; documentation comments
  keep a distinct token kind without semantic attachment.
- Add the first deterministic textual encoder (syntax) with golden tests; identical input bytes
  must produce byte-identical encodings.
- Migrate the syntax inspector lab to read `SyntaxFile` directly: token stream including trivia,
  surface tree, and missing/error nodes highlighted.

## Capabilities

### New Capabilities

- `bootstrap-syntax-file`: The per-module lossless syntax artifact — bytes, tokens, surface tree,
  stable IDs — and its deterministic textual encoder.

### Modified Capabilities

- `bootstrap-source-text`: Source identity becomes part of the `SyntaxFile` artifact contract.
- `bootstrap-lexer`: Tokens are owned by and identified within a `SyntaxFile`.
- `bootstrap-syntax`: The surface tree is owned by and identified within a `SyntaxFile`.
- `bootstrap-syntax-inspector`: The syntax lab consumes the `SyntaxFile` artifact.

## Impact

Lexer/parser public shapes, all downstream consumers of parse results (semantic analysis,
evaluator, inspector flow model), syntax fixtures, and golden tests. Grammar is unchanged.

## Plan References

- [Roadmap — Track 1, proposal 2](../../../roadmaps/compiler-realignment.md)
- [Issue 06](../../../wayfinder/bootstrap-language/issues/06-bootstrap-compiler-pipeline.md),
  frontend paragraph: "The frontend begins with a lossless `SyntaxFile` for every loaded source
  module. It owns the original source bytes, a token stream retaining whitespace and comments,
  and a source-faithful surface tree with explicit missing and error nodes. Tree nodes and tokens
  carry stable source IDs and byte spans."
- Same ticket, encoders paragraph: "Syntax, HIR, and MIR … may each expose an ordinary,
  deterministic textual encoder for debugging, inspection, and golden tests."
- Same ticket, determinism gate: "identical … inputs must produce byte-identical syntax, HIR, and
  MIR textual encodings."
