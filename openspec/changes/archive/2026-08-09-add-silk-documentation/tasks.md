## 1. Compiler documentation source model

- [x] 1.1 Add exact `//!` module-documentation tokenization, exhaustive token consumers, and lexer tests while preserving `///` and ordinary `//` behavior.
- [x] 1.2 Add the compiler `DocBlock` actor with raw module/declaration attachment queries, exact token/span provenance, blank-line and ordinary-comment boundaries, and tests for every supported declaration level.
- [x] 1.3 Expose raw documentation through `Analysis` for modules, canonical declarations, parameters, fields, and implementation operations, with definition/reference and cross-module tests.
- [x] 1.4 Update canonical formatting to retain module and nested declaration documentation attachment, with round-trip tests.

## 2. Optional documentation package

- [x] 2.1 Scaffold `@silklang/docgen` with explicit actor subpaths, workspace configuration, CommonMark dependency ownership, and package-level test configuration.
- [x] 2.2 Implement the immutable `Document` model and total lazy parser: marker normalization, package-owned CommonMark nodes, source provenance, `Examples` discovery, and malformed-content fallback.
- [x] 2.3 Implement best-effort Rust-style intra-document links against compiler scope facts, resolving canonical identities and degrading unresolved links to inline code without diagnostics.
- [x] 2.4 Implement the immutable `Project` model with source-ordered modules/declarations, compiler-derived signatures, child documentation, and public-by-default visibility filtering.
- [x] 2.5 Implement deterministic experimental `Json` encoding with fixed field order, logical provenance, no leaked compiler/parser objects or absolute paths, and byte-stability tests.

## 3. Editor and hover adapters

- [x] 3.1 Integrate the shared documentation model into LSP definition/reference hover, preserving signature-only intrinsic and anonymous-expression fallback and testing full examples.
- [x] 3.2 Add documentation-owned highlight ranges to CodeMirror for markers, CommonMark constructs, intra-document links, and nested fenced Silk code without changing ordinary compiler analysis.
- [x] 3.3 Extend the TextMate grammar and generated VS Code grammar with distinct module/declaration scopes and nested documentation markup tests.

## 4. JSON generation workflow

- [x] 4.1 Add `silk doc` command parsing, output and `--include-private` options, help text, and command-model tests.
- [x] 4.2 Implement project documentation analysis and atomic JSON destination writing without backend invocation or partial output, with success, visibility, determinism, and source-rejection tests.

## 5. Package and repository handoff

- [x] 5.1 Document the source syntax, package interfaces, hover behavior, JSON IR, experimental compatibility status, and deferred doctest behavior; add required changesets and explicit package exports.
- [x] 5.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`, fixing change-caused failures and recording any pre-existing failures.
