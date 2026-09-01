## 1. SyntaxFile artifact

- [x] 1.1 Create `packages/compiler/src/SyntaxFile.ts`: the immutable `SyntaxFile` (source, token
      stream, surface tree, lexical + parser diagnostic collections) with a `make` bundler
- [x] 1.2 Implement derived stable identities: pre-order walk at construction,
      `{sourceId, ordinal}` identity shape, lookup answering `None` for foreign elements
- [x] 1.3 Change `Parser.parse` to return `SyntaxFile`; delete the `ParseResult` shape; export
      `SyntaxFile` from the package index and exports map

## 2. Documentation comments

- [x] 2.1 Lex `///` comments as the distinct `DocComment` token kind (same coverage rule as line
      comments); add `DocComment` to the parser's trivia kinds
- [x] 2.2 Lexer tests: doc comment vs line comment distinction, final-line coverage, and the
      fixture determinism scenarios stay green

## 3. Downstream migration

- [x] 3.1 Re-shape `SemanticAnalysis`: `analyze(syntax: SyntaxFile)`, `Result.syntax` replacing
      `Result.parse`; update all internal field paths
- [x] 3.2 Update compiler tests (parser, semantic, syntax-tree, evaluation) to the artifact shape
- [x] 3.3 Update the release-candidate consumer script and API surface assertions
      (`./SyntaxFile` export, no `ParseResult`)

## 4. Deterministic textual encoder

- [x] 4.1 Implement `SyntaxFile.encode`: header, token stream, indented tree with explicit
      missing/error entries, diagnostics; `\xNN` escaping for non-printable bytes
- [x] 4.2 Golden tests: committed accepted and malformed golden files compared byte-for-byte, plus
      a repeat-encoding determinism test

## 5. Inspector

- [x] 5.1 Migrate the syntax lab to read the `SyntaxFile` artifact (source, tokens, tree,
      diagnostics field paths)
- [x] 5.2 Add the trivia-inclusive token stream view; keep missing/error highlighting sourced from
      the same artifact
- [x] 5.3 Update inspector tests: token stream presence, artifact-sourced highlighting

## 6. Verification

- [x] 6.1 Full compiler and docs test suites pass; `pnpm check` green
- [x] 6.2 `openspec validate establish-syntax-file-artifact --type change --strict` passes
