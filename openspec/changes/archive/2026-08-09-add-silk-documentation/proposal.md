## Why

Silk already preserves and highlights `///` tokens, but documentation is not attached to semantic
declarations, shown in hover, or available to documentation generators. Silk needs one lazy,
Markdown-native documentation model that keeps ordinary compilation fast while giving editor and
generation tooling the same source of truth.

## What Changes

- Add declaration documentation with `///` and module documentation with `//!`, retained as raw,
  source-owned comment blocks by the compiler without parsing Markdown during lexing or CST parsing.
- Add an optional `@silklang/docgen` package that lazily and totally parses CommonMark,
  recognizes Rust-style intra-document links, preserves examples and provenance, and falls back to
  readable text for malformed content or unresolved links.
- Enrich symbol hover with the complete declaration documentation, including examples, while
  continuing to derive signatures, parameters, return types, failures, and requirements from
  compiler semantic facts.
- Add documentation-aware CodeMirror and TextMate highlighting without putting Markdown work on the
  compiler's ordinary analysis path.
- Generate a deterministic, formatter-neutral JSON documentation IR. Public declarations are
  included by default and private declarations require an explicit option. The bootstrap schema is
  experimental and carries no compatibility guarantee.
- Add a `silk doc` workflow that emits the JSON IR for external Markdown, HTML, terminal, and site
  formatters.
- Defer executable documentation tests while retaining fenced Silk examples and exact source
  provenance needed by a later doctest workflow.

## Capabilities

### New Capabilities

- `silk-documentation-model`: Raw documentation attachment, lazy total CommonMark parsing,
  intra-document links, example preservation, and the formatter-neutral documentation project
  model.
- `silk-documentation-json`: Deterministic experimental JSON generation, visibility selection, and
  formatter-facing semantic identities and source provenance.

### Modified Capabilities

- `bootstrap-lexer`: Recognize `//!` module documentation distinctly from `//` and `///`.
- `bootstrap-syntax-file`: Expose source-owned raw documentation blocks without parsing their body.
- `bootstrap-analysis-facade`: Make raw documentation attachment queryable through the supported
  tooling facade.
- `silk-source-formatting`: Preserve module documentation and documentation attached at every
  declaration level.
- `language-server-hover`: Render complete declaration documentation beside the semantic signature.
- `language-codemirror`: Highlight documentation markers and nested Markdown structure through the
  optional documentation tooling.
- `language-textmate`: Scope both declaration and module documentation, including useful nested
  Markdown constructs.
- `silk-cli-workflows`: Add deterministic JSON documentation generation through `silk doc`.

## Impact

- Affected packages: `@silklang/compiler`, `@silklang/editor-support`, `@silklang/lsp`,
  `@silklang/cli`, and a new `@silklang/docgen` package.
- The compiler token vocabulary and public subpath exports gain documentation actors and a module
  documentation token kind.
- LSP hover output changes from a signature-only Markdown code block to a signature followed by the
  complete authored documentation when available.
- Package contents and exports change, requiring release-candidate validation and changeset coverage.
- The normal parse and analysis pipeline does not acquire a Markdown dependency or documentation
  failure mode.
