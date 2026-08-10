## 1. Literal Form Model and Vocabulary

- [x] 1.1 Add the compiler literal-form actor with immutable descriptors for category, modifier, delimiter width, and escape policy; cover longest recognized and unknown-modifier introduction matching with focused tests.
- [x] 1.2 Export the tooling-safe literal-form metadata through the compiler barrel and package subpath while keeping byte-scanning mechanics private.
- [x] 1.3 Add the `InvalidStaticLiteral` token kind, source-language descriptions, lexical diagnostic codes/reasons for unknown modifiers and unterminated delimiters, and diagnostic identity/order tests.

## 2. Canonical Lexer and Lossless Syntax

- [x] 2.1 Replace hard-coded literal-start handling in the TypeScript lexer with one-pass form recognition and scanning for all four committed forms, including escaped delimiters and exact token spans.
- [x] 2.2 Implement bounded single-line recovery, EOF-bounded multiline recovery, and unknown-modifier reservation with one lexical diagnostic and no duplicate invalid-byte diagnostics.
- [x] 2.3 Parse valid and invalid static-literal tokens as one lossless expression position without parser cascades, retaining modifiers, delimiter width, content, trivia, and spans.
- [x] 2.4 Add lexer/parser regression cases for all literal forms, LF/CRLF bodies, code-like content, escaped triple quotes, unknown modifiers, both unterminated boundaries, and all four forms as pipeline operands.
- [x] 2.5 Extend lexer pressure and determinism fixtures so the expanded vocabulary retains stable throughput, fingerprints, contiguous coverage, and repeated-process results.

## 3. Atomic Static Literal Decoding

- [x] 3.1 Refactor static-literal decoding to consume a recognized form instead of a byte-string boolean and to strip the correct modifier and delimiter widths.
- [x] 3.2 Implement exact multiline body decoding with no dedent or structural newline trimming, physical CRLF-to-LF normalization, explicit `\r\n` preservation, and invalid physical backslash-newline handling.
- [x] 3.3 Preserve atomic escape, Unicode, UTF-8, and byte-range failure behavior while preventing lexical delimiter/modifier sentinels from producing duplicate semantic diagnostics.
- [x] 3.4 Add static-data identity, HIR/MIR, evaluation, checked-indexing, native LLVM, and direct WebAssembly parity coverage for multiline text and bytes.

## 4. Silk Lexer Differential Parity

- [x] 4.1 Update the Silk-written lexer token/diagnostic vocabulary and literal scanner to match the canonical form, boundary, and unknown-modifier contract without compiler or backend special cases.
- [x] 4.2 Expand the differential corpus with valid and invalid single/multiline literals, escapes, modifiers, LF/CRLF, pipeline punctuation, and unterminated recovery; assert token and diagnostic parity.
- [x] 4.3 Run the representative lexer pressure cases across evaluation, native LLVM, and direct WebAssembly, including allocation-failure cleanup ordinals introduced by the new scan paths.

## 5. Content-Aware Formatting

- [x] 5.1 Add a verbatim multiline formatter document variant whose fit and render logic normalizes CRLF, preserves semantic horizontal whitespace, and tracks columns after embedded line endings.
- [x] 5.2 Route complete triple-delimited tokens through the verbatim document path while leaving ordinary text invariants and damaged-syntax rejection intact.
- [x] 5.3 Add formatter tests for unequal indentation, leading/final newlines, trailing spaces inside content, escaped delimiters, following same-line syntax, decoded-value preservation, and byte-identical second formatting.

## 6. CodeMirror, TextMate, and VS Code

- [x] 6.1 Extend CodeMirror category and UTF-8/UTF-16 range coverage for valid multiline literals and invalid-literal tokens across edits without adding a second boundary scanner.
- [x] 6.2 Generate longest-first TextMate rules from literal-form metadata with stable scopes for modifiers, delimiters, bodies, and escapes in all four committed forms.
- [x] 6.3 Add Shiki tokenizer tests proving that multiline bodies retain string scope across comments, keywords, punctuation, non-ASCII content, escaped triple quotes, and physical line endings.
- [x] 6.4 Regenerate the checked-in VS Code grammar and verify structural equality and behavioral highlighting parity with the package grammar.

## 7. Repository Verification

- [x] 7.1 Run `pnpm typecheck` and resolve every new compiler, language-package, pressure-program, and generated-artifact type error.
- [x] 7.2 Run `pnpm exec biome check .` and resolve formatting or lint findings without suppressions.
- [x] 7.3 Run `pnpm test`, including differential and cross-engine literal coverage, and resolve regressions.
- [x] 7.4 Run `pnpm check` as the complete handoff gate.
- [x] 7.5 Run `pnpm release:candidate` because compiler token vocabulary and public package exports change, and report any failure with its exact command and provenance.
