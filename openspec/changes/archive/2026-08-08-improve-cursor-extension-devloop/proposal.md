## Why

Local Silk editor support in Cursor is fragile and slow to iterate: the documented symlink install points at an absolute package path, so worktrees (and deleted agent checkouts) leave a dangling extension; there is no Extension Development Host launch config; and the TextMate grammar only paints a coarse keyword blob. Developers rebuild and reload without confidence that Cursor is loading _this_ checkout, and highlighting is merely adequate when it could read more like a real language.

## What Changes

- Add a **worktree-safe Cursor install** path that retargets the local symlink to the current checkout, builds the extension (and its LSP dependency), and documents reload vs. language-server restart.
- Add an **Extension Development Host** launch configuration (plus build/watch tasks) so F5 loads `apps/vscode` from the open workspace without touching `~/.cursor/extensions`.
- Enrich the **TextMate grammar** with finer scopes (control vs storage keywords, function names, broader types/identifiers) so themes color Silk more usefully — still generated from `@silklang/editor-support`, still drift-checked.
- Update extension README / contributor docs for the two workflows: daily use in main Cursor vs. developing the extension/LSP in a guest host window.
- **Non-goals:** marketplace/OpenVSX packaging, LSP semantic tokens, tree-sitter, theme contributions, Changesets publication of the private extension.

## Capabilities

### New Capabilities

- `extension-dev-host`: Workspace launch and task definitions that open a VS Code / Cursor Extension Development Host against `apps/vscode`, with builds that keep the host and language server on current `dist` output.

### Modified Capabilities

- `cursor-extension`: Replace hand-maintained absolute symlink instructions with a retargetable local install that always points at the current checkout; clarify reload vs. `Silk: Restart Language Server`.
- `language-textmate`: Require richer, theme-friendly scopes beyond a single `keyword.other` alternation while preserving keyword parity with the compiler and existing match/generics coverage.

## Impact

- `apps/vscode` — install script, README, possibly package scripts; no marketplace packaging.
- `.vscode/launch.json` and `.vscode/tasks.json` — Extension Development Host + preLaunch / watch tasks.
- `packages/editor-support` — `TextMate.ts` patterns/scopes, sync script output, TextMate tests / Shiki fixtures.
- Generated `apps/vscode/syntaxes/silk.tmLanguage.json` and `language-configuration.json` via existing `sync:vscode`.
- Docs site Shiki highlighting inherits the richer grammar automatically.
- Does not change `@silklang/lsp` protocol surface; LSP restart remains the thin extension command already present.
