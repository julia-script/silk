# Tasks — add-language-package

## 1. Package scaffolding

- [x] 1.1 Create `packages/editor-support` (`@silklang/editor-support`): package.json with subpath exports
      for `./CodeMirror` and `./TextMate`; tsconfig pair matching sibling packages; CodeMirror
      packages as peer + dev dependencies; wire into pnpm workspace and turbo.
- [x] 1.2 Verify `pnpm build` and `pnpm check` pass with the empty package in the graph.

## 2. TextMate grammar

- [x] 2.1 Define the Silk TextMate grammar (scope `source.silk`) in `src/TextMate.ts`: keywords,
      `//` line comments, `///` doc comments, decimal integers, operators, punctuation. (The
      grammar's source of truth is TypeScript — keyword alternations are built from a
      type-exhaustive `Record<KeywordKind, string>`; JSON is generated for the editor extension.)
- [x] 2.2 Define the language configuration in `src/TextMate.ts`: `//` line comment, `()`/`{}`
      brackets, auto-closing pairs.
- [x] 2.3 `src/TextMate.ts`: typed exports of the grammar and language configuration.
- [x] 2.4 `test/TextMate.test.ts`: keyword-sync test — lex every declared keyword spelling through
      the compiler lexer and assert its token kind; assert the grammar's keyword alternations
      equal the compiler keyword set; tokenize samples with Shiki and assert
      keyword/boolean/number/comment/doc-comment scopes.

## 3. CodeMirror extension

- [x] 3.1 `src/CodeMirror.ts`: byte→UTF-16 offset mapping with ASCII fast path; `TokenKind` →
      highlight tag/class mapping (doc comments and invalid tokens distinct); StateField that
      re-lexes on change and builds the DecorationSet; exported extension factory + base theme.
- [x] 3.2 `test/CodeMirror.test.ts`: decoration positions for a keyword/number/comment sample;
      invalid-token styling; non-ASCII document places `fn` highlight correctly; re-lex on edit.

## 4. Docs app integration

- [x] 4.1 Register the Silk grammar in `apps/docs/lib/source.ts` via `rehypeCodeOptions.langs`;
      add a ` ```silk ` fence to a docs page and confirm highlighting in light and dark themes,
      with `ebnf` fallback still working.
- [x] 4.2 Replace the workbench `SourceBody` textarea with a CodeMirror view: doc changes →
      `modulesAtom`; nonempty selection → `cursorAtom` (UTF-16 → byte offsets); active-module
      switches and URL/preset resets reconcile editor state; footer and styling preserved via
      workbench theme variables.
- [x] 4.3 Verify workbench behavior in the browser: highlighted editing updates downstream panes,
      selection lights up spans, URL round-trips source.

## 5. Cursor extension

- [x] 5.1 Create `apps/vscode` (private): manifest contributing the `silk` language for
      `.silk`, grammar, and language configuration; sync script copying both files from
      `packages/editor-support`; test asserting copies are byte-identical to the source files;
      README documenting the symlink install; exclude from Changesets/release-candidate validation.
- [x] 5.2 Symlink into `~/.cursor/extensions/` and verify a `.silk` file highlights in Cursor.

## 6. Finish

- [x] 6.1 Run `pnpm build`, `pnpm check`, and the full test suite; fix fallout.
- [x] 6.2 Update root README package list with `@silklang/editor-support`.
