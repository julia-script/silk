## Context

See proposal.md. `DeclarationCollection.collect` and `DeclarationCompletion.complete` are the phase entry points. Facts, resolution, completion, and conformance callers import their owning actors directly; `DeclarationIndex` retains only index-coordinator behavior and no forwarding facade.

## Decisions

- **Five actors** along the existing seams: DeclarationFacts.ts (28–1324 + lookups), DeclarationCollection.ts (analyzeDeclaredType 1506–2340, collectModule 3116–3895), DeclarationResolution.ts (3916–5757), DeclarationCompletion.ts (complete 5818–7100 + its passes), ConformanceProof.ts (prove/conforms/witness/copyProof 7102–8088).
- **analyzeAppliedRows(source, list, typeParameters)**: returns failures/requirements/requirementParameters/rowDiagnostics; both the Effect special case and the generic branch consume it.
- **collectRowExpression(source, syntax, typeParameters, leaf)**: the structural RowWithout/UnionType/unavailable walk shared once, with the leaf discriminated (failure member vs row/requirement member).
- **Imports**: the five actors import SourceSpan/SyntaxTree/Type/Diagnostic directly; keep any NameResolution <-> DeclarationIndex cycles type-only.

## Risks / Trade-offs

- [Diagnostic parity] -> the shared row parser must reproduce current kind-mismatch/unknownType diagnostics exactly; diagnostic-code tests are the gate.
- [Large motion] -> one actor per commit.

## Validation

pnpm typecheck, pnpm exec biome check ., pnpm test (declaration-index + diagnostics suites).
