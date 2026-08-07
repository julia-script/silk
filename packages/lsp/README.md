# @silk-effect/lsp

A Language Server Protocol server for Silk Effect, powered by the bootstrap compiler's analysis
snapshot. Project-scoped analysis sessions feed every feature; the server adds only protocol
translation.

## Features

- **Diagnostics** — every compiler phase (lexical, parser, module, semantic, ownership) with stable
  codes, notes, and cross-file related information.
- **Hover** — the inferred type of the smallest typed expression or binding under the cursor.
- **Document symbols** — top-level functions and structs, with struct fields as children.
- **Formatting** — whole-document canonical formatting via the compiler's formatter.
- **Go to definition** — precise local, parameter, declaration, imported, qualified, and field
  navigation, including declarations in open unsaved or closed project modules.

Open documents are analyzed as compilation roots against their discovered `silk.toml` project, so
imports resolve to sibling open documents first and rooted `.silk` files second. Documents outside
any project fall back to their own directory as the source root; virtual documents receive isolated
stable identities.

Analysis is coalesced per project with one latest-wins worker. A semantic request waits for the
exact synchronized document version it captured rather than interpreting a current position
against stale text or compiler facts. Diagnostics are published atomically for the newest accepted
project revision and include the analyzed document version.

The server dynamically registers standard LSP watchers for `**/*.silk` and `**/silk.toml` when the
client supports watched-file registration. Closed dependency edits invalidate only containing
projects, while open editor buffers continue to take precedence over disk. Manifest changes
rediscover project membership for affected open documents. Clients that do not report watched-file
events still receive correct open-buffer behavior, but external disk changes are not visible until
another synchronization event.

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
