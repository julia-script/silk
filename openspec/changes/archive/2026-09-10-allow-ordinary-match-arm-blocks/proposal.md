## Why

Match arms currently require a single expression, so sequential eager work requires a helper or a
constructed and immediately run Effect. JUL-105 requests ordinary arm-local statements without the
capture, allocation, and execution-boundary semantics of an Effect.

## What Changes

- Accept an ordinary statement block immediately after a match arm's `=>`, alongside existing
  expression arms, in every expression position that accepts a match.
- Execute the selected block eagerly in the enclosing computation. Normal completion contributes
  unit; a block with no normally completing path contributes `never`. Preserve existing reachable
  result joining, including rejection of unit mixed with an incompatible scalar.
- Preserve lexical scope, enclosing return and loop targets, Effect failure and requirement rows,
  and ownership cleanup across transfers nested in larger expressions.
- Carry explicit expression-or-block arm bodies and completion facts through analysis, HIR,
  specialization, MIR lowering, static evaluation, and formatting.
- Update reference documentation and focused semantic, structural, and native acceptance evidence.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-syntax`: Lossless expression-or-statement-block arm parsing and bounded recovery.
- `bootstrap-exhaustive-matching`: Unit/bottom block completion and reachable result joining.
- `bootstrap-semantic-facts`: Enclosing return contracts, statement scope, flow, and Effect rows
  discovered through expression-nested arm bodies.
- `bootstrap-hir`: Explicit typed arm body kinds, provenance, completion, and transfer regions.
- `bootstrap-ownership`: Selected-arm scope and cleanup with continuation-only ownership joins.
- `bootstrap-mutable-loops`: Lexical transfers through expression-nested ordinary arm blocks.
- `bootstrap-mir`: Expression evaluation and match result storage restricted to completing paths.
- `bootstrap-backend`: LLVM realizes selected statement arms and early exits from MIR.
- `static-evaluation`: Eager arm statements and enclosing transfers during compile-time execution.
- `silk-source-formatting`: Canonical statement arm layout with stable trivia and damaged-input
  handling.

## Impact

The compiler parser, semantic analysis, HIR visitors and encoders, ownership traversal,
specialization, MIR/LLVM lowering, StaticEvaluation, formatter, and compiler test corpus change.
The control-flow and Effect reference pages describe the new behavior. Existing diagnostic codes
and spans remain the intended vocabulary; catalog examples change only where needed. No dependency,
new intrinsic, runtime evaluator, or independent Wasm backend is introduced.

## Non-goals

General block expressions, trailing-expression values, a separate match statement, implicit Effect
construction, and changes to explicit callable/Effect execution boundaries are excluded. JUL-106's
retained if-let issue remains separate.

Issue: https://linear.app/juliaortiz/issue/JUL-105/allow-ordinary-statement-blocks-in-match-arms
