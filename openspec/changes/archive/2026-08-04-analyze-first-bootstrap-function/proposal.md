## Why

The compiler can now recover one function as lossless syntax, but it still cannot say what the
function declares, what `I32` means, or whether `42` is a valid returned value. This is the smallest
point at which to discover a semantic fact model from real evidence without prematurely creating
an AST, HIR, or generalized analysis framework.

## What Changes

- Analyze the existing one-function parse result into immutable declaration, type, integer-value,
  and return-compatibility facts with exact syntax provenance.
- Recognize only the built-in signed 32-bit integer type `I32`, parse decimal integer values exactly,
  and diagnose unknown return types and out-of-range literals deterministically.
- Keep unavailable facts explicit when syntax is missing or damaged, retain the original parse
  result, and expose semantic diagnostics separately without duplicating lexical or parser errors.
- Extend the direct-link-only Syntax Inspector with a compact semantic-facts and semantic-diagnostics
  view for the accepted and nearby malformed fixtures.
- Preserve the boundary before multiple declarations, references, general expressions, semantic
  AST lowering, HIR, MIR, ownership, contracts, and code generation.

## Capabilities

### New Capabilities

- `bootstrap-semantic-facts`: Deterministic declaration and `I32` facts for the first parsed
  function, including explicit unavailable states and semantic diagnostics.

### Modified Capabilities

- `bootstrap-syntax-inspector`: Show the new semantic facts and diagnostics while preserving the
  inspector's hidden, local, disposable character.

## Impact

- Adds a narrow semantic-analysis boundary and public actor namespaces to `@silk-lang/compiler`.
- Adds focused valid and malformed semantic fixtures and packed-export validation.
- Updates the existing docs lab client bundle and presentation without adding persistence, network
  requests, or runtime dependencies.
- Does not change the accepted grammar, concrete tree, lexical diagnostics, or parser diagnostics.
