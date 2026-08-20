## Why

Silk has nominal match forms but no single pattern representation shared by match arms, local destructuring, and conditional destructuring. The confirmed language requires one compiler-defined, non-executable pattern system with exact union membership, ownership-aware access, irrefutability, and `if let`.

## What Changes

- Create one parsed and semantic pattern representation reused by `match`, `let`, and `if let`.
- Add recursive irrefutable local destructuring with move, shared-borrow, exclusive-borrow, and copy behavior.
- Keep `let _ = value` from bypassing explicit `drop` for non-unit results.
- Add statement-form `if let` with success bindings, optional mismatch body, consume-on-both outcomes for moves, loan-scoped borrows, cleanup, and flow diagnostics.
- Admit exact normalized nominal and non-nominal union member patterns with static coverage, narrowing, and deterministic monomorphic renormalization.
- Leave irrefutable-condition simplification to the language service rather than compiler rejection.

## Capabilities

### Modified Capabilities

- `bootstrap-syntax`: parse one shared pattern grammar in match, let, and if-let positions.
- `bootstrap-exhaustive-matching`: generalize coverage and narrowing to exact ordinary union members.
- `bootstrap-structural-unions`: expose static member evidence to pattern analysis.
- `bootstrap-ownership`: assign one ownership action and cleanup plan to every binding path.
- `bootstrap-hir`: carry typed patterns and flow refinements.
- `bootstrap-mir`: lower deterministic tests, bindings, joins, and cleanup.

## Impact

Depends on `generalize-borrows-and-callable-lifetimes` and `generalize-ordinary-structural-unions`. It changes parser, formatter, semantic patterns, coverage, ownership, HIR/MIR, engines, diagnostics, LSP, and tests. Patterns remain non-callable and create no user-defined matching protocol.
