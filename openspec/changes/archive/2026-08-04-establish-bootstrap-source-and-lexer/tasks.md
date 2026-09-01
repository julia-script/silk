## 1. Compiler Package

- [x] 1.1 Create `packages/compiler` with strict ESM TypeScript, build, typecheck, Vitest, and
      package metadata consistent with the workspace.
- [x] 1.2 Add the compatible `effect` runtime and `@effect/vitest` development dependencies through
      the workspace catalog and update the lockfile.
- [x] 1.3 Add explicit root namespace exports and package subpaths only for `SourceFile`,
      `SourceSpan`, `Token`, `LexicalDiagnostic`, and `Lexer`.
- [x] 1.4 Add built-package tests that import every declared root and deep public API.

## 2. Source Bytes and Spans

- [x] 2.1 Implement `SourceFile` with caller-supplied logical identity and a defensive immutable
      snapshot of the input `Uint8Array`.
- [x] 2.2 Implement opaque owner-qualified `SourceSpan` values with half-open byte offsets, empty
      end-of-file spans, readonly accessors, and equality.
- [x] 2.3 Implement exact source-byte access and span slicing that returns no bytes for a span owned
      by another source file.
- [x] 2.4 Test invalid UTF-8, zero bytes, mixed line endings, input-buffer mutation, empty sources,
      valid empty and non-empty spans, exact slicing, and foreign-span rejection.

## 3. Lossless Lexer

- [x] 3.1 Implement the `Token` actor and closed kernel token-kind vocabulary for whitespace, line
      comments, keywords, identifiers, decimal integers, punctuation, invalid regions, and EOF.
- [x] 3.2 Implement the `LexicalDiagnostic` actor and lexing result with stable invalid-region code,
      source-owned primary span, deterministic message, and ordered readonly collections.
- [x] 3.3 Implement contiguous whitespace and `//` line-comment scanning while keeping the following
      line ending in a separate whitespace token.
- [x] 3.4 Implement ASCII identifier scanning with whole-token keyword classification, decimal digit
      runs, single-byte punctuation, and longest-match `->` recognition.
- [x] 3.5 Implement maximal invalid-region recovery, exact invalid tokens and diagnostics, guaranteed
      forward progress, and one empty EOF token.
- [x] 3.6 Document and benchmark the single index-based per-byte scan as the lexer’s measured
      performance-critical imperative inner loop.
- [x] 3.7 Test the first parser fixture, keyword prefixes, trivia-heavy input, final comments,
      unsupported ASCII and UTF-8 bytes, recovery between valid tokens, full byte reconstruction, and
      repeated deterministic results.

## 4. Documentation and Verification

- [x] 4.1 Add source and lexer fixtures with human-readable expected token kinds, byte spans, slices,
      and diagnostics.
- [x] 4.2 Document the package API, exact-byte and half-open-span conventions, supported kernel
      vocabulary, and every token family deliberately deferred to later changes.
- [x] 4.3 Extend release-candidate validation to pack and import the compiler package without source
      files or undeclared runtime dependencies.
- [x] 4.4 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and
      `pnpm release:candidate`, fixing every introduced failure and reporting any proven pre-existing
      failure exactly.
