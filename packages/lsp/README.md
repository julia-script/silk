# @silk-effect/lsp

A Language Server Protocol server for Silk Effect, powered by the bootstrap compiler's analysis
snapshot. One analysis pass per document change feeds every feature; the server adds only protocol
translation.

## Features

- **Diagnostics** — every compiler phase (lexical, parser, module, semantic, ownership) with stable
  codes, notes, and cross-file related information.
- **Hover** — the inferred type of the smallest typed expression or binding under the cursor.
- **Document symbols** — top-level functions and structs, with struct fields as children.
- **Formatting** — whole-document canonical formatting via the compiler's formatter.

Open documents are analyzed as compilation roots against their discovered `silk.toml` project, so
imports resolve to sibling open documents first and rooted `.silk` files second. Documents outside
any project fall back to their own directory as the source root.

## Usage

The package ships a stdio binary:

```sh
silk-lsp
```

Point any LSP client at it. For example, in Neovim:

```lua
vim.lsp.config('silk', {
  cmd = { 'silk-lsp' },
  filetypes = { 'silk' },
  root_markers = { 'silk.toml' },
})
```

The actors (`Document`, `Workspace`, `LineIndex`, `Server`) are also exported as subpaths for
embedding the same analysis-to-protocol translation elsewhere.
