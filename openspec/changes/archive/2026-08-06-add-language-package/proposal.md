# Add `@silk-lang/language` package

## Why

Silk source is currently edited and displayed with no syntax highlighting anywhere: the labs
workbench uses a bare `<textarea>`, docs code fences render as plain text, and `.silk` files open in
Cursor as plain text. The compiler already ships a browser-safe TypeScript lexer, so editor-grade
highlighting can be derived from the real token stream instead of a duplicated grammar.

## What Changes

- New workspace package `@silk-lang/language` (`packages/language`) providing:
  - A lexer-driven CodeMirror 6 extension that highlights Silk by running the compiler's bootstrap
    lexer (`@silk-lang/compiler/Lexer`) and mapping `TokenKind` to highlight styles. No Lezer
    grammar; the compiler lexer is the single source of truth.
  - A Silk TextMate grammar (`silk.tmLanguage.json`) plus a language configuration (brackets,
    comments, auto-closing pairs), exported for consumption by Shiki and by editors.
  - A keyword-sync test that derives the keyword list from the compiler's `TokenKind` union and
    asserts the TextMate grammar covers exactly those keywords.
- The labs workbench source pane replaces its `<textarea>` with a CodeMirror editor using the new
  extension, preserving existing behavior (URL-encoded source, span cursor on selection).
- The docs app registers the TextMate grammar with fumadocs/Shiki so ` ```silk ` code fences
  highlight across the docs site.
- A private, declarative Cursor/VS Code extension (`packages/vscode`, `private: true`): manifest
  contributing the `silk` language for `.silk` files, the TextMate grammar, and the language
  configuration. No extension code, no `vsce`, no marketplace — installed by symlinking the folder
  into `~/.cursor/extensions/`. Excluded from Changesets and release-candidate validation.

Out of scope: tree-sitter grammar (its own toolchain and package; a later change), semantic
highlighting, diagnostics-in-editor, LSP.

## Capabilities

### New Capabilities

- `language-codemirror`: Lexer-driven CodeMirror 6 highlighting extension for Silk.
- `language-textmate`: Silk TextMate grammar and language configuration, exported for Shiki and
  editors, with keyword coverage verified against the compiler's `TokenKind`.
- `docs-silk-highlighting`: Workbench CodeMirror editor and Shiki-highlighted ` ```silk ` fences in
  the docs app.
- `cursor-extension`: Private declarative Cursor/VS Code extension registering the Silk language.

### Modified Capabilities

None — the compiler's specs are unchanged; this change only consumes the existing lexer.

## Impact

- New packages: `packages/language` (publishable later, consumed via `workspace:*` for now) and
  `packages/vscode` (private).
- `apps/docs`: new dependency on `@silk-lang/language` and CodeMirror packages; workbench source
  pane and MDX/Shiki configuration change.
- New third-party dependencies: `@codemirror/state`, `@codemirror/view`, `@codemirror/language`,
  `@lezer/highlight` (style tags only).
- `@silk-lang/compiler` is unchanged but gains a new consumer of `Lexer` and `Token`.
