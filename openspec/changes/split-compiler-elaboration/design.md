## Context

See proposal.md. `elaborateModule` is a synchronous pure entry (`SyntaxTree` → facts → HIR). `ElaborationResult`, the fact unions, and `elaborateModule` are the stable surface; the four execution phases read them.

## Decisions

- **`ExpressionAnalysis`**: `analyzeExpression` + the `analyze*Literal/Identifier/Move/Borrow/Match/Struct/Array/Projection/Operator/Pipeline/Effect/Run` sub-actors (1408–8094). Pull the three inline mega-branches into named `analyzeEffectBlock`/`analyzeRun`/`analyzeCall`.
- **`CallResolution`**: the call-resolution tail (8566–9030) plus `analyzeCallTypeArguments`/`analyzeArguments`/`analyzeCallContract`/`solveCallableConstraints`/`seededSpecialization`/`resolvedFunctionReference`/`serviceOperation`/`boundOperationReference`/`finish*Call`/`analyzeBuiltinCall`/`finishCallableSection`/`finishCallableApplication`/`finishIntrinsicContractCall`.
- **`StatementAnalysis`**: `analyzeStatements`, `analyzeFunctionBody`, `returnFlowOf`, `executableStatements`, `effectCaptureFacts`.
- **`HirLowering`**: `hirExpression` (10603–11615), `hirExpectedExpression`, `hirReference`, `hirPatternSelection`, `hirWritePlace`, and the merged `lowerStatements`.
- **Statement lowering merge**: one `lowerStatements(facts, { resultType, functionId, eraseIntrinsicSections, borrowBindingInitializers })`; delete `hirEffectStatements` and route the closure through it, keeping the closure's stricter behavior.
- **`strongestEffectAccess`**: the one Take>Exclusive>Shared reducer used by all five sites.
- **`argumentBorrowId(argument, ordinal)` + `loanEndsOf`**: consumed by every call-lowering branch.

## Risks / Trade-offs

- [Drift reconciliation] → the merged statement lowering changes behavior for a couple of `UnavailableStatement`/Write edges; capture before/after with the golden determinism suite.
- [Import cycles] → the four actors import the `Elaboration` fact vocabulary; keep cycles type-only.

## Validation

`pnpm typecheck`, `pnpm exec biome check .`, `pnpm test` (elaboration/determinism golden suites).
