## 1. Syntax and Recovery

- [x] 1.1 Add parser regressions for standalone `run foo()`, consecutive expression statements,
  identifier-call versus assignment dispatch, and preservation of the implicit or explicit unit
  return without phantom declarations.
- [x] 1.2 Add `ExpressionStatement` to concrete syntax and parse remaining expression starts as
  statements after explicit statement and assignment dispatch.
- [x] 1.3 Add damaged-expression and unexpected-punctuation regressions that require recovery to
  retain the following statement and owning right brace inside the current block.
- [x] 1.4 Implement block-owned error-statement synchronization so one malformed statement cannot
  escape into top-level declaration recovery or publish dependent missing-token cascades.
- [x] 1.5 Extend unexpected-syntax structured reasons and messages with encountered token kinds,
  parser context, and source-language expectations; update diagnostic model tests and snapshots.
- [x] 1.6 Format expression statements through ordinary expression formatting and add comment,
  adjacency, idempotence, and syntax-correspondence coverage.

## 2. Semantic Facts and HIR

- [x] 2.1 Add elaboration tests for unit and `never` expression statements, non-unit scalar and owned
  results, and unavailable expressions that must not receive duplicate diagnostics.
- [x] 2.2 Add the expression-statement semantic fact and a dedicated non-unit-result diagnostic that
  retains the actual type and offers bind, return, or explicit-consumption guidance.
- [x] 2.3 Add the HIR `Evaluate` statement with expression, region, span, and unavailable-cause
  preservation, without creating binding, return, or drop semantics.
- [x] 2.4 Update HIR validation, unavailable discovery, expression traversal, deterministic encoding,
  statement queries, semantic occurrence collection, call-target discovery, and analysis projections
  for `Evaluate`.
- [x] 2.5 Update layout and instance/type discovery traversals for the expression carried by
  `Evaluate`, and add deterministic analysis/HIR regression coverage.

## 3. Ownership, Lowering, and Execution

- [x] 3.1 Add ownership regressions proving evaluate expressions participate in use, move, borrow,
  run-site propagation, loan-ending, and cleanup analysis without inventing a live binding.
- [x] 3.2 Update ownership statement roots and run-boundary scanning to include `Evaluate` and retain
  the same propagation exit plans as equivalent bind or return expressions.
- [x] 3.3 Lower `Evaluate` by executing its expression exactly once, forwarding only after successful
  unit completion, and preserving terminal `never` or propagated-failure outcomes.
- [x] 3.4 Add evaluator, native, and direct WebAssembly parity tests for ordered successful unit
  effects, continuation to following statements, and a declared failure that halts effectful `main`
  after performing required cleanup.

## 4. Tooling and Verification

- [x] 4.1 Update syntax-inspector, presentation, CLI/LSP diagnostic, and documentation fixtures that
  expose statement kinds or the enriched unexpected-syntax payload.
- [x] 4.2 Run focused parser, formatter, elaboration, HIR, ownership, lowering, entry-termination, and
  cross-engine tests and resolve every failure attributable to the change.
- [x] 4.3 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and
  `pnpm release:candidate`, reporting any pre-existing failure separately.
- [x] 4.4 Run strict OpenSpec validation and confirm every scenario in both delta specs has direct
  automated evidence.
