## Why

The bootstrap language has settled syntax and compiler constraints, but the repository has no Silk
source model or lexer. Before planning syntax trees, semantic IRs, or native code generation, the
compiler needs one trustworthy first capability: preserve source bytes and turn them into a
deterministic token stream with precise lexical diagnostics.

## What Changes

- Add the initial `@silk-lang/compiler` package and only the public actors needed to represent a
  source file, byte span, token, lexical diagnostic, and lexing result.
- Preserve the exact bytes and explicit logical identity of one in-memory Silk source file.
- Lex the subset needed to spell the first future parser fixture: whitespace, line comments, `pub`, `fn`,
  `return`, identifiers, decimal integer literals, `(`, `)`, `{`, `}`, `->`, and end-of-file.
- Preserve trivia and invalid input in the token stream with exact byte spans rather than discarding
  or normalizing source text.
- Report invalid bytes as stable lexical diagnostics while continuing to the next safe token
  boundary.
- Add deterministic lexer fixtures and tests over valid, trivia-heavy, malformed, and byte-edge
  inputs.
- Explicitly defer parsing, syntax trees, declarations, name resolution, types, HIR, MIR, LLVM,
  filesystem/process boundaries, and native execution to later changes.

## Capabilities

### New Capabilities

- `bootstrap-source-text`: Defines exact source-byte ownership, logical source identity, and valid
  byte spans for every later compiler phase.
- `bootstrap-lexer`: Defines the first permanent Silk token vocabulary, trivia preservation,
  lexical diagnostics, recovery, and deterministic tokenization.

### Modified Capabilities

None. This change does not alter the existing LLVM capabilities.

## Impact

- Adds `packages/compiler` as `@silk-lang/compiler` with strict TypeScript, ESM, explicit package
  subpaths, and `@effect/vitest` coverage.
- Adds source and lexer fixtures under the compiler package.
- Adds the compiler package to workspace build, typecheck, test, and packed-package validation.
- Introduces no filesystem, process, Clang, LLVM, or runtime integration and does not modify the
  Tiny-language tutorial.
