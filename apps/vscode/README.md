# Silk Language (Cursor / VS Code extension)

Syntax highlighting and language server support for `.silk` files. No marketplace — the extension
runs straight from this repository.

Two parts:

- **Grammar and language configuration** are generated from `@silklang/editor-support` (the single
  source of truth) by `pnpm --filter @silklang/editor-support sync:vscode`; a test in that package
  fails when the generated files drift.
- **Language server**: `src/extension.ts` owns the `@silklang/lsp` child process and gives its
  detached stdio transport to `vscode-languageclient`. Diagnostics, hover types, go to definition,
  document symbols, formatting (including `editor.formatOnSave`), and dynamic project file watchers
  all come from the server. The stable editor session gates diagnostics by client generation and
  editor version, and only becomes ready after built-in document synchronization is acknowledged.

## Silk Inspector

**Silk: Open Inspector** (command palette) opens one webview panel beside the editor showing a
compiler-phase view of the active `.silk` file — tokens, concrete tree, HIR, ownership, MIR,
backend output, the whole pipeline — one view at a time, switched with the in-panel picker. The
panel follows the active `.silk` editor, refreshes when the language server commits a new
analysis, and shares a span cursor with the editor: selecting source tints the rows covering it,
and clicking a row reveals its span (opening the owning module's file when it differs).

All projection happens in the language server (`silk/inspectorView`); the panel renders rows and
nothing else. Inspection is static and never executes the active program. Views that need target
realization (layout, MIR, backend, toolchain) are rooted at the active file.

## Which workflow?

| Goal                                      | Use                                                       |
| ----------------------------------------- | --------------------------------------------------------- |
| Edit `.silk` in your normal Cursor window | [Install (local)](#install-local)                         |
| Develop the extension or language server  | [Extension Development Host](#extension-development-host) |

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

The extension resolves `@silklang/lsp` through the workspace `node_modules`, so the checkout
must stay installed and built.

## Extension Development Host

To iterate on the extension or LSP without touching the global extensions directory, launch
**Silk: Extension Development Host** from the Run and Debug view (F5). That opens a guest window
with `--extensionDevelopmentPath` set to this package; a pre-launch task builds `@silklang/lsp`
and `silk-language` first.

Optional watch tasks (**Silk: Watch language server**, **Silk: Watch extension**) rebuild on save
while the host is open.

### LSP acceptance launch

Run **Silk: LSP Acceptance Host (manual)** from Run and Debug. It builds both packages and opens the
fixture workspace at `apps/vscode/test/fixtures/lsp-acceptance` in an Extension Development
Host. In `src/Main.silk`:

1. On the blank line in `main`, type `effects`, pause for diagnostics and hover, shorten it to
   `effec`, change it to `Effect.`, then delete the line so the file is valid again.
2. Open Quick Fix while an intermediate spelling is present. After every edit, the Problems entry
   and hover must either describe that exact spelling or disappear; an older `effec` diagnostic
   must never reappear, and `Loading...` / quick-fix checks must settle without reloading the window.
3. In `src/Util.silk`, rename `answer` to `answer2`. The unchanged call in `Main.silk` must become
   diagnostic-bearing after the watched dependency refresh. Restore `answer`; the diagnostic must
   clear.
4. Run **Silk: Restart Language Server** while hover or Quick Fix is active. Restart must finish,
   the open documents must resynchronize once, and current diagnostics and hover must recover
   without **Developer: Reload Window**.

The stdio and scheduler suites automate the same source revisions, cancellation, dependency
refresh, process wedging, and recovery. This launch is the opt-in production-adapter check through
the real editor extension host.

For the scripted form, run **Silk: LSP Extension Host Test**. The guest host drives the same edits
through VS Code's real `vscode-languageclient` adapter, checks synchronous diagnostic retirement,
pull-diagnostic generation gating, dependency refresh, hover/inlay-hint/quick-fix completion,
acknowledged restart, and diagnostic removal on close, then exits with a failing launch when an
assertion does not hold.

## Reload vs restart

| Change                                                                                        | Action                                                                                                                               |
| --------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------ |
| Retargeted install, grammar (`sync:vscode`), or `extension.ts` / `package.json` contributions | **Developer: Reload Window** (main Cursor or the EDH guest)                                                                          |
| Rebuilt `@silklang/lsp` only, same extension path                                             | **Silk: Restart Language Server** — retires the current client and starts a fresh server from the new `dist` without a window reload |

**Silk: Restart Language Server** also recovers when the old server is unresponsive. The stable
editor session gives protocol cleanup one bounded retirement window, forcibly terminates a process
that does not exit, acknowledges its death, and only then starts the replacement. Use **Developer:
Reload Window** only for extension-host changes in the first row, or if the extension itself cannot
execute commands; an LSP stop-timeout no longer requires a reload.
