## Why

Silk has lossless concrete syntax but no canonical source representation, leaving formatting choices
to each author, tool, and future AI workflow. A single strict formatter is needed now so the CLI and
future language server can share deterministic source output without accumulating style options or
editor-specific behavior.

## What Changes

- Add a strict, width-aware formatter over the lossless `SyntaxFile` artifact that rejects lexical
  or parser damage while remaining independent of semantic analysis.
- Establish one canonical Silk source style: a 100-column target, two-space indentation, LF line
  endings, one final newline, no trailing whitespace, deterministic list breaking and trailing
  commas, and bounded blank-line preservation.
- Preserve comment spelling except terminal horizontal whitespace and define an immediately
  preceding `///` block as documentation for a declaration or field.
- Return deterministic formatted bytes plus whether the source changed, with no public style
  options and no cursor or filesystem responsibility in the formatter.
- Add `silk format` for project-wide formatting, optional positional file or directory selection,
  and `--check` verification with deterministic reporting and existing CLI exit classes.
- Keep future LSP text-edit conversion, range/on-type formatting, and diagnostic autofixes as
  adapters or later capabilities rather than responsibilities of whole-document formatting.

## Capabilities

### New Capabilities

- `silk-source-formatting`: Canonical source layout, strict formatter behavior, comment and trivia
  policy, CLI selection/check/write workflows, and the reusable formatter boundary for future IDEs.

### Modified Capabilities

None.

## Impact

- Adds public formatter and formatted-document actors and explicit package exports under
  `packages/compiler`.
- Adds formatting workflow, command, filesystem traversal, reporting, exports, tests, and
  documentation under `packages/compiler-cli`.
- Extends parser/formatter contract tests around documentation comments, idempotence, source
  equivalence, width boundaries, and damaged syntax.
- Establishes a future LSP integration boundary without adding an LSP implementation or a new
  runtime dependency in this change.
