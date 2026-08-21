## Why

`Elaboration.ts` is the package's largest file: 12,635 lines, 271 top-level declarations, no section banners, spanning five phases (fact vocabulary, syntax→fact analysis, call resolution, statement-flow, HIR lowering). Its two expression/statement walks have already drifted, borrow-id construction is copy-pasted six-plus times, and effect-access reduction is computed five different ways.

## What Changes

- **Keep `Elaboration.ts`** as the fact vocabulary + `Result` + the `elaborateModule` façade; **extract four execution actors**: `ExpressionAnalysis`, `CallResolution`, `StatementAnalysis`, and `HirLowering`.
- **Merge the duplicated statement lowering** — `hirEffectStatements` (10429–10585) and the `hirStatements` closure (12271–12468) — into one parameterized `lowerStatements`.
- **Add single helpers** for borrow-id construction (`argumentBorrowId`/`loanEndsOf`) and effect-access reduction (`strongestEffectAccess`), replacing the 6× and 5× inline copies.
- **Remove the two redundant `as SyntaxTree.Node` casts** by binding `arms.at(0)` once.

## Capabilities

### New Capabilities

<!-- none -->

### Modified Capabilities

<!-- none: behavior-preserving refactor (skip_specs) -->

## Impact

Pure refactor of the front-middle-end semantics pass; no observable behavior change except where the drifted statement lowering is reconciled (the merged helper picks the closure's more complete behavior — the Borrow-id initializer and Mutable-write guard — verified against the full suite). `skip_specs: true`.
