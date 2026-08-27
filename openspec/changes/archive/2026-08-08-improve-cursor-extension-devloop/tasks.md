## 1. Retargetable local install

- [x] 1.1 Add a Node install script under `apps/vscode` that builds `@silklang/lsp` and `silk-language`, then `ln -sfn` the package folder to `~/.cursor/extensions/silk-effect.silk-language-0.0.0`
- [x] 1.2 Wire `install:cursor` (and optional `install:code` / `--vscode` for `~/.vscode/extensions`) in `apps/vscode/package.json`
- [x] 1.3 Make the script print the resolved symlink target and remind the user to reload the window; exit non-zero if build or link fails
- [x] 1.4 Rewrite `apps/vscode/README.md` install section to use the script instead of a hand `ln -s`, and document reload vs `Silk: Restart Language Server`

## 2. Extension Development Host

- [x] 2.1 Add `.vscode/tasks.json` entries to build (and optionally watch) `@silklang/lsp` and `silk-language`, including a compound pre-launch build task
- [x] 2.2 Add an `extensionHost` launch configuration in `.vscode/launch.json` with `--extensionDevelopmentPath=${workspaceFolder}/apps/vscode` and the pre-launch build task
- [x] 2.3 Document the EDH workflow in `apps/vscode/README.md` (when to use EDH vs `install:cursor`; restart LS vs reload host for grammar/activation changes)

## 3. Richer TextMate scopes

- [x] 3.1 Split keyword spellings in `packages/editor-support/src/TextMate.ts` into control vs declaration/storage families with distinct scopes; keep boolean literals separate
- [x] 3.2 Add a `fn` + identifier pattern that scopes function declaration names as `entity.name.function`
- [x] 3.3 Broaden type-like PascalCase scoping for ordinary type positions beyond builtins and nominal `Name{` patterns, with conservative heuristics to limit false positives
- [x] 3.4 Extend TextMate/Shiki tests for the new scope families and preserve compiler keyword-parity checks
- [x] 3.5 Run `sync:vscode` and confirm generated `apps/vscode/syntaxes/silk.tmLanguage.json` matches the editor-support package export

## 4. Verification

- [x] 4.1 Run `pnpm typecheck`, package tests for `@silklang/editor-support`, and `pnpm exec biome check` on touched paths
- [x] 4.2 Manually verify `install:cursor` replaces a wrong/dangling symlink and that Reload Window loads `.silk` highlighting from this checkout
- [x] 4.3 Manually verify Extension Development Host launches with Silk active even when the global symlink is absent or wrong
- [x] 4.4 Spot-check highlighting on a sample `.silk` file (keywords, `fn` name, user types, match/generics still sane) in Cursor or the EDH window
