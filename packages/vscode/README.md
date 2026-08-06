# Silk Language (Cursor / VS Code extension)

Declarative syntax highlighting for `.silk` files. No extension code, no marketplace — the grammar
and language configuration are generated from `@silk-effect/language` (the single source of truth)
by `pnpm --filter @silk-effect/language sync:vscode`, and a test in that package fails when the
generated files drift.

## Install (local, symlink)

```sh
ln -s "$(pwd)/packages/vscode" ~/.cursor/extensions/silk-effect.silk-language-0.0.0
```

Reload Cursor (`Developer: Reload Window`) and open a `.silk` file. For VS Code, use
`~/.vscode/extensions` instead. Edits to the grammar show up after regenerating and reloading the
window — no packaging step.
