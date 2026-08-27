## Context

See proposal.md for motivation. Constraints that shape the approach:

- `apps/vscode` is a private app (`silk-language`) whose README documents a hand symlink into `~/.cursor/extensions`. That absolute path breaks across worktrees; a dangling symlink to a deleted agent worktree was observed in the wild.
- Extension `main` is `./dist/extension.js`; `@silklang/lsp` is a workspace dependency resolved via `require.resolve('@silklang/lsp/bin')`. Both need a built checkout.
- Grammar SoT remains `packages/editor-support/src/TextMate.ts`, synced into the extension by `pnpm --filter @silklang/editor-support sync:vscode` and drift-checked in tests. Docs Shiki consumes the same grammar.
- `.vscode/launch.json` today only runs Silk programs; there is no `extensionHost` configuration.
- The extension already exposes `silk.restartLanguageServer` for server-only iteration.
- Existing specs: `cursor-extension`, `language-textmate`. This change adds `extension-dev-host`.

## Goals / Non-Goals

**Goals:**

- One command retargets the local editor install to *this* checkout and builds what it needs.
- F5 / Extension Development Host loads the workspace extension without touching the global extensions dir.
- Clear split: restart LS vs reload window.
- Richer TextMate scopes that themes actually map, without abandoning the shared SoT or keyword-parity tests.

**Non-Goals:**

- Marketplace / `.vsix` publication, OpenVSX, iconography.
- LSP semantic tokens (deferred; TextMate enrichment only).
- Hot-reloading TextMate grammars without a window reload (editor limitation).
- Changing LSP protocol features or analysis behavior.

## Decisions

### 1. Two supported workflows, one package

```
  Daily Silk editing          Extension / LSP development
  ─────────────────          ───────────────────────────
  install:cursor             Extension Development Host
       │                              │
       ▼                              ▼
  ~/.cursor/extensions           --extensionDevelopmentPath
  symlink → this checkout        = ${workspaceFolder}/apps/vscode
       │                              │
       └──────── reload / restart LS ─┘
```

- **Why both:** EDH alone does not highlight `.silk` in the main Cursor window where people write Silk. Symlink alone is what broke on worktrees. Keep both; document which to use when.
- **Alternative considered:** Only EDH — rejected; daily language work happens in the main window.
- **Alternative considered:** Only fix the symlink — rejected; EDH is the right loop for extension/LSP changes and avoids global-install cache confusion.

### 2. Install script retargets a stable extension id folder

- Script (Node, package script on `silk-language`, e.g. `install:cursor`) resolves the absolute path of `apps/vscode`, ensures `pnpm` builds `@silklang/lsp` and `silk-language` (or relies on turbo filter build), then `ln -sfn` into `~/.cursor/extensions/silk-effect.silk-language-0.0.0` (and optionally `~/.vscode/extensions/...` via a flag or sibling script).
- Use `ln -sfn` so dangling or wrong-worktree links are replaced atomically.
- Keep version `0.0.0` and folder naming stable so the entry is uniquely identifiable; do not introduce marketplace versioning.
- Print the resolved target path and a one-line "Reload Window" reminder; mention `Silk: Restart Language Server` for server-only rebuilds.
- **Alternative considered:** Bump a local build timestamp in `package.json` on each install to bust caches — deferred unless Reload Window proves insufficient after retargeting; prefer not to dirty the tree.
- **Alternative considered:** Pack a `.vsix` each time — heavier, was a non-goal in the original language-package design; revisit only if symlink+reload stays flaky on Cursor.

### 3. Extension Development Host launch + tasks

- Add a launch config: `type: extensionHost`, `request: launch`, `args: ["--extensionDevelopmentPath=${workspaceFolder}/apps/vscode"]`, with `preLaunchTask` that builds extension + lsp (and syncs grammar if needed).
- Prefer composing existing package `build` / `dev` scripts via `tasks.json` (`dependsOn` for lsp + vscode; optional compound watch).
- Document: contribution/grammar → reload host; server `dist` → `Silk: Restart Language Server`.
- **Cursor note:** Cursor is VS Code-compatible for `extensionHost` launches when debugging from the repo; if a given Cursor build refuses EDH, the same `launch.json` still serves VS Code, and `install:cursor` remains the main-window path.
- **Alternative considered:** CLI-only `cursor --extensionDevelopmentPath=...` wrapper — optional later; launch.json is the standard discoverable entry.

### 4. TextMate enrichment stays regex SoT, not lexer-in-extension

Improve `TextMate.ts` patterns:

| Concern | Approach |
| --- | --- |
| Keyword families | Split keyword list into control (`if`/`else`/`return`/`while`/…) vs storage/declaration (`pub`/`fn`/`struct`/`let`/…) with scopes like `keyword.control.silk` and `storage.type.silk` / `keyword.declaration.silk` |
| Function names | Match `fn` + identifier capture → `entity.name.function.silk` |
| Types | Broaden PascalCase type matching beyond builtins and `Name{` patterns (signature/type positions), keeping builtins if useful for consistency |
| Parity tests | Keep compiler keyword-set equality; add Shiki/scope fixture assertions for the new families |

- Still generate JSON via `sync:vscode`; extension remains a consumer.
- **Why not LSP semantic tokens yet:** more protocol + theme surface; proposal defers them. TextMate upgrades help Cursor and Shiki immediately from one SoT.
- **Why not drive VS Code highlighting from the compiler lexer:** VS Code expects TextMate or semantic tokens; embedding the lexer in the extension would fork the CodeMirror approach and fight the editor model.
- **Alternative considered:** Only remap existing `keyword.other` without new patterns — helps themes less than adding function/type entity scopes.

### 5. Docs updates, not new packages

- Update `apps/vscode/README.md` as the source of truth for install + EDH + reload/restart.
- No new workspace package; scripts live under `apps/vscode` (install) and reuse `packages/editor-support` sync.

## Risks / Trade-offs

- **[Risk] Cursor EDH quirks** → Mitigation: keep `install:cursor` as the reliable main-window path; verify EDH on Cursor once during apply; fall back to VS Code for extension debugging if needed.
- **[Risk] Broader PascalCase type rules false-positive on constants** → Mitigation: scope conservatively (prefer known type positions / existing pattern heuristics); add fixture tests for false friends; prefer under-coloring over wrong coloring.
- **[Risk] Symlink still requires Reload Window** → Mitigation: document explicitly; do not promise hot grammar reload.
- **[Risk] Workspace `node_modules` resolution for `@silklang/lsp` from a symlinked extension** → Mitigation: install script builds from repo root; document that the checkout must remain `pnpm install &&` built; EDH loads from workspace so resolution matches the open monorepo.
- **[Trade-off] No `.vsix`** → Simpler alpha workflow; harder to share installs outside the repo. Acceptable at this stage.

## Migration Plan

1. Land install script + README; contributors with a dangling symlink run `install:cursor` once and reload.
2. Land EDH launch/tasks; no migration for people who only use the symlink.
3. Land TextMate scope changes + `sync:vscode`; reload editor / EDH to see new highlighting; docs site picks up grammar on next docs build.
4. Rollback: revert change; re-run old manual symlink if needed. No data migration.

## Open Questions

- Whether to also write `~/.vscode/extensions` by default or only behind `--vscode` / `install:code` (default Cursor-only is fine unless dual-editor use is common).
- Exact TextMate scope strings (e.g. `storage.type` vs `keyword.declaration`) — pick during apply to match common theme mappings; specs require differentiated families, not a particular string.
