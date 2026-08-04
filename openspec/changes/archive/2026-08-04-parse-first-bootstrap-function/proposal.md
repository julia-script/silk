## Why

The bootstrap compiler can now preserve and tokenize arbitrary source bytes, but tokens alone do
not express even the smallest Silk program or give later phases a recoverable grammatical
structure. The next evidence-producing step is one deliberately tiny function-and-return grammar
whose source-faithful tree can be inspected before semantic representations are introduced.

## What Changes

- Parse exactly one public, parameterless function whose named return type and body contain one
  decimal-integer return statement, followed by end-of-file.
- Publish an immutable, lossless concrete syntax tree that retains every lexer token, including
  whitespace, comments, invalid tokens, and EOF, exactly once.
- Represent missing required syntax and unexpected tokens explicitly, with deterministic parser
  diagnostics and bounded recovery for the first malformed-input corpus.
- Add a small interactive Syntax Inspector to the docs site that shows the accepted fixture,
  concrete tree, token spans, source slices, and diagnostics at a direct-link-only route.
- Keep the inspector and public API honest about the boundary: this is concrete syntax, not a
  semantic AST, and names such as `main` and `I32` remain uninterpreted identifiers.

## Capabilities

### New Capabilities

- `bootstrap-syntax`: Lossless parsing, concrete syntax identity, bounded missing/error recovery,
  and deterministic parser diagnostics for the first function grammar.
- `bootstrap-syntax-inspector`: A hidden-from-navigation docs page that lets developers inspect the
  first fixture and small edits as concrete syntax, tokens, spans, slices, and diagnostics.

### Modified Capabilities

None.

## Impact

- Extends `@silk-effect/compiler` with explicit syntax, parser-diagnostic, and parser actors and
  their package subpath exports.
- Adds parser fixtures and malformed-input coverage alongside the existing source and lexer tests.
- Adds `@silk-effect/compiler` as a workspace dependency of the docs app and introduces a small
  client-side inspector at a stable direct URL without adding it to normal navigation.
- Does not add general expressions, parameters, multiple declarations, semantic AST lowering,
  name or type analysis, HIR, MIR, LLVM lowering, filesystem loading, or compilation commands.
