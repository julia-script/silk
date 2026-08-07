# Silk Language (Cursor / VS Code extension)

Syntax highlighting and language server support for `.silk` files. No marketplace — the extension
runs straight from this repository.

Two parts:

- **Grammar and language configuration** are generated from `@silk-effect/language` (the single
  source of truth) by `pnpm --filter @silk-effect/language sync:vscode`; a test in that package
  fails when the generated files drift.
- **Language server**: `src/extension.ts` forks the `@silk-effect/lsp` stdio server for `silk`
  documents via `vscode-languageclient`. Diagnostics, hover types, document symbols, and
  formatting (including `editor.formatOnSave`) all come from the server; the extension itself
  stays a thin launcher.

## Install (local, symlink)

```sh
pnpm build
ln -s "$(pwd)/packages/vscode" ~/.cursor/extensions/silk-effect.silk-language-0.0.0
```

Reload Cursor (`Developer: Reload Window`) and open a `.silk` file. For VS Code, use
`~/.vscode/extensions` instead. The symlinked extension resolves `@silk-effect/lsp` through the
workspace's `node_modules`, so it needs a built checkout (`pnpm install && pnpm build`) — after
changing server code, rebuild and reload the window.
