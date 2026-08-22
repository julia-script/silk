## Context

See proposal.md. `DeclarationCollection.collect` and `DeclarationCompletion.complete` are the phase entry points. Facts, resolution, completion, and conformance callers import their owning actors directly; `DeclarationIndex` retains only index-coordinator behavior and no forwarding facade.

## Decisions

- **Coordinator and five actors**: `DeclarationIndex.ts` owns only the assembled `Index` data contract and orchestration seam. `DeclarationFacts.ts` owns the fact vocabulary and fact lookups, `DeclarationCollection.ts` owns syntax collection and collection helpers, `DeclarationResolution.ts` owns resolution queries, `DeclarationCompletion.ts` owns completion and its passes, and `ConformanceProof.ts` owns conformance/type-copy operations. There are no compatibility forwarding exports.
- **analyzeAppliedRows(source, list, typeParameters)**: returns failures/requirements/requirementParameters/rowDiagnostics; both the Effect special case and the generic branch consume it.
- **collectRowExpression(source, syntax, typeParameters, leaf)**: the structural RowWithout/UnionType/unavailable walk shared once, with the leaf discriminated (failure member vs row/requirement member).
- **Imports**: the five actors import SourceSpan/SyntaxTree/Type/Diagnostic directly. They may import the `Index` data type from `DeclarationIndex`, but never import collection, fact, resolution, or conformance behavior from the coordinator; keep any NameResolution <-> DeclarationIndex cycles type-only.

## Risks / Trade-offs

- [Diagnostic parity] -> the shared row parser must reproduce current kind-mismatch/unknownType diagnostics exactly; diagnostic-code tests are the gate.
- [Large motion] -> one actor per commit.

## Validation

pnpm typecheck, pnpm exec biome check ., pnpm test (declaration-index + diagnostics suites).
