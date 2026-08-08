# Silk Language (Cursor / VS Code extension)

Syntax highlighting and language server support for `.silk` files. No marketplace — the extension
runs straight from this repository.

Two parts:

- **Grammar and language configuration** are generated from `@silk-effect/language` (the single
  source of truth) by `pnpm --filter @silk-effect/language sync:vscode`; a test in that package
  fails when the generated files drift.
- **Language server**: `src/extension.ts` forks the `@silk-effect/lsp` stdio server for `silk`
  documents via `vscode-languageclient`. Diagnostics, hover types, go to definition, document
  symbols, formatting (including `editor.formatOnSave`), and dynamic project file watchers all
  come from the server; the extension itself stays a thin launcher. The bundled language client
  supports standard dynamic watched-file registration, so no duplicate extension-owned watcher is
  installed.

## Install (local, symlink)

```sh
pnpm build
ln -s "$(pwd)/packages/vscode" ~/.cursor/extensions/silk-effect.silk-language-0.0.0
```

Reload Cursor (`Developer: Reload Window`) and open a `.silk` file. For VS Code, use
`~/.vscode/extensions` instead. The symlinked extension resolves `@silk-effect/lsp` through the
workspace's `node_modules`, so it needs a built checkout (`pnpm install && pnpm build`) — after
changing server code, rebuild and reload the window.
