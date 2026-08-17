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

## Silk Inspector

**Silk: Open Inspector** (command palette) opens one webview panel beside the editor showing a
compiler-phase view of the active `.silk` file — tokens, concrete tree, HIR, ownership, MIR,
backend output, the whole pipeline — one view at a time, switched with the in-panel picker. The
panel follows the active `.silk` editor, refreshes when the language server commits a new
analysis, and shares a span cursor with the editor: selecting source tints the rows covering it,
and clicking a row reveals its span (opening the owning module's file when it differs).

All projection happens in the language server (`silk/inspectorView`); the panel renders rows and
nothing else. Evaluation is explicit — the run button on the evaluation view — and is cleared by
the next edit. Views that need a runtime realization (layout, MIR, backend, toolchain) are rooted
at the active file.

## Which workflow?

| Goal | Use |
| --- | --- |
| Edit `.silk` in your normal Cursor window | [Install (local)](#install-local) |
| Develop the extension or language server | [Extension Development Host](#extension-development-host) |

## Install (local)

From the repo root (or this package), retarget the editor symlink to **this** checkout and build
the extension plus language server:

```sh
pnpm --filter silk-language install:cursor
```

For VS Code instead of Cursor:

```sh
pnpm --filter silk-language install:code
```

Both: `pnpm --filter silk-language exec node scripts/install-local.mjs --vscode`.

Then reload the editor (`Developer: Reload Window`) and open a `.silk` file. The symlink lives at
`~/.cursor/extensions/silk-effect.silk-language-0.0.0` (or `~/.vscode/extensions/...`) and always
points at the checkout where you ran the command — re-run it after switching git worktrees.

The extension resolves `@silk-effect/lsp` through the workspace `node_modules`, so the checkout
must stay installed and built.

## Extension Development Host

To iterate on the extension or LSP without touching the global extensions directory, launch
**Silk: Extension Development Host** from the Run and Debug view (F5). That opens a guest window
with `--extensionDevelopmentPath` set to this package; a pre-launch task builds `@silk-effect/lsp`
and `silk-language` first.

Optional watch tasks (**Silk: Watch language server**, **Silk: Watch extension**) rebuild on save
while the host is open.

## Reload vs restart

| Change | Action |
| --- | --- |
| Retargeted install, grammar (`sync:vscode`), or `extension.ts` / `package.json` contributions | **Developer: Reload Window** (main Cursor or the EDH guest) |
| Rebuilt `@silk-effect/lsp` only, same extension path | **Silk: Restart Language Server** — picks up the new `dist` without a window reload |
