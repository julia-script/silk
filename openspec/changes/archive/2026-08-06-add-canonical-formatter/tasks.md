## 1. Formatter Foundation

- [x] 1.1 Add the public `FormattedDocument` data actor with immutable formatted bytes and a
      `changed` flag, plus explicit compiler barrel and package subpath exports.
- [x] 1.2 Add the formatter's typed damaged-syntax error model and the public
      `Formatter.format(SyntaxFile)` Effect boundary without filesystem, project, option, cursor, or LSP
      inputs.
- [x] 1.3 Implement the private byte-oriented document algebra and renderer for text, hard and soft
      lines, concatenation, indentation, groups, and break-only content.
- [x] 1.4 Test document rendering at exact and exceeded 100-column boundaries, nested indentation,
      deterministic group decisions, trailing-whitespace prevention, and the canonical final newline.

## 2. Syntax Safety and Trivia

- [x] 2.1 Implement complete-syntax validation for lexical diagnostics, parser diagnostics, missing
      tokens, and unexpected-token error regions before layout begins.
- [x] 2.2 Add strict-formatting tests for valid syntax, semantic-only errors, invalid tokens, missing
      tokens, and parser error regions, asserting that damaged input produces no replacement bytes.
- [x] 2.3 Implement the centralized trivia classifier for same-line trailing comments, standalone
      comments, author-supplied blank-line signals, and immediately attached `///` documentation blocks.
- [x] 2.4 Test comment spelling preservation, terminal horizontal-whitespace normalization, and
      source ordering across trailing, standalone, attached documentation, unattached documentation,
      nested-block, delimiter, and end-of-file boundaries.
- [x] 2.5 Test top-level one-blank-line separation, interior at-most-one blank-line preservation,
      adjacent statement layout, and removal of leading and trailing blank lines.

## 3. Canonical Syntax Printing

- [x] 3.1 Implement exhaustive grammar-directed printing for file structure, declarations, imports,
      types, and struct fields using canonical spaces and punctuation.
- [x] 3.2 Implement exhaustive grammar-directed printing for blocks, statements, expressions,
      operators, calls, member access, literals, arrays, and struct initializers without changing source
      order or operator grouping.
- [x] 3.3 Implement reusable delimited-list layout for parameters, arguments, import members, array
      elements, and struct initializers, with compact comma-space forms and one-item-per-line broken
      forms containing trailing commas.
- [x] 3.4 Add golden formatter fixtures covering every current `NodeKind`, canonical LF and
      two-space indentation, nested width decisions, preserved over-width tokens/comments, and all
      formatter-controlled optional comma positions.
- [x] 3.5 Add fixture-corpus assertions that formatted output reparses without syntax diagnostics,
      preserves the normalized grammatical program and comment sequence, and formats a second time to
      identical bytes with `changed: false`.

## 4. CLI Selection and Storage

- [x] 4.1 Add the `FormatWorkflow` actor and result model for sorted per-file outcomes: unchanged,
      changed, damaged, and storage failure.
- [x] 4.2 Implement default recursive discovery of exact `.silk` files beneath the selected
      project's source root without using entry-module reachability or following directory symlinks.
- [x] 4.3 Implement positional file and directory restriction, source-root containment checks,
      resolved-file escape rejection, target deduplication, and canonical path sorting.
- [x] 4.4 Implement sequential per-file read, parse, strict format, and classification so damaged or
      failed files do not prevent later safely readable files from being classified.
- [x] 4.5 Implement write mode using temporary sibling files and atomic rename for changed files only,
      including cleanup and typed storage/write failures; ensure check mode performs no write calls.
- [x] 4.6 Add workflow tests for whole-root selection including unreachable files, explicit files and
      directories, duplicate paths, outside-root and symlink escapes, deterministic ordering, damaged
      file continuation, atomic writes, and check-only behavior.

## 5. Command Integration

- [x] 5.1 Add and register the `silk format` Effect CLI command with positional paths, `--check`, and
      the existing project-manifest selection behavior.
- [x] 5.2 Implement deterministic terminal reporting and aggregate exit classes `0`, `1`, and `2`,
      with the highest encountered class winning after all safely processable files are reported.
- [x] 5.3 Add command integration tests for successful writes, canonical checks, check drift, damaged
      syntax, semantic-only errors, invalid selection, read failures, and write failures.

## 6. Documentation and Verification

- [x] 6.1 Document the canonical Silk style, strict refusal behavior, default project scope,
      positional selection, and `silk format --check` CI usage.
- [x] 6.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`, resolving
      failures introduced by the formatter change and recording any pre-existing failures.
- [x] 6.3 Run `pnpm release:candidate` to verify the new compiler exports and package contents.
