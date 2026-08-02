## Why

Arithmetic expressions alone do not constitute the confirmed language. The parser must grow into a complete program parser for function definitions, parameters, calls, comparisons, and expression-valued conditionals.

## What Changes

- Add `Program` data and complete `Parser.parse` for the confirmed Tiny grammar.
- Parse function definitions, parameter and argument lists, name references, calls, `<`, `>`, and `if/then/else`.
- Reject incomplete or trailing syntax with typed, source-spanned parse diagnostics.
- Reject duplicate parameter names at the owning phase.
- Add a full AST snapshot for `score.tiny` and focused invalid-syntax tests.

## Capabilities

### New Capabilities

None. This change adds documentation and tutorial-example material, so the change opts out of behavioral specs with `skip_specs: true`.

### Modified Capabilities

None.

## Impact

Adds tutorial parser implementation, program data, fixtures, and tests. It remains independent of LLVM lowering.

