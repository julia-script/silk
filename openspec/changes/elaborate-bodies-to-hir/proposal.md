## Why

The spike stops at semantic facts over concrete syntax; the pinned pipeline requires a real HIR —
resolved, typed, generic-aware, desugared from surface grammar — built by one integrated
elaboration phase, because a function's contract is part of its type. This is the largest single
realignment step: the remaining body-analysis half of the `SemanticAnalysis` monolith is replaced
by elaboration that constructs HIR and publishes fact tables keyed by canonical IDs. The
fact idiom with explicit unavailable states survives; the monolith's structure does not.

## What Changes

- **BREAKING**: Replace `SemanticAnalysis` body analysis with an integrated elaboration phase —
  local and referenced-name resolution, expression and pattern typing, contract inference or
  validation, and HIR construction as one phase, walking the declaration dependency graph.
  Private non-recursive dependencies may be memoized on demand; recursive strongly connected
  components require their explicit contracts (degenerate in the frozen slice, but the structure
  is real).
- Introduce HIR: canonical declaration and type IDs, normalized function contracts, core semantic
  operations with source provenance, retaining type and contract-row parameters.
- Publish type facts and function-contract facts as immutable tables in the analysis snapshot;
  unknown facts stay explicit and never masquerade as valid empty contracts or concrete types.
- Add the HIR deterministic textual encoder with golden tests.
- Re-point the evaluator to elaboration output as a mechanical migration (its move to MIR is a
  later proposal); the inspector flow model keeps working throughout.
- Add inspector coverage: HIR view for a selected declaration, and type-on-hover over source
  spans.

## Capabilities

### New Capabilities

- `bootstrap-hir`: The resolved, typed, generic-aware semantic representation, its construction
  by integrated elaboration, its fact tables, and its textual encoder.

### Modified Capabilities

- `bootstrap-semantic-facts`: Superseded by elaboration fact tables; the explicit-unavailability
  idiom carries over onto canonical IDs.
- `bootstrap-evaluation`: Consumes elaboration output instead of the monolith's result.
- `bootstrap-syntax-inspector`: HIR view and type hover; flow model reads elaboration facts.

## Impact

Deletes the `SemanticAnalysis.ts` monolith (1,143 lines) in favor of the phase pair introduced
here and in `collect-declaration-headers`. Touches the evaluator, the inspector flow model, all
semantic fixtures, and golden tests. No grammar changes.

## Plan References

- [Roadmap — Track 2, proposal 5](../../../roadmaps/compiler-realignment.md)
- [Issue 06](../../../wayfinder/bootstrap-language/issues/06-bootstrap-compiler-pipeline.md),
  frontend checking order, step 3: "Local and referenced-name resolution, expression and pattern
  typing, function-contract inference or validation, handler row subtraction, and HIR
  construction are one integrated phase because a function's contract is part of its type."
- Same ticket, IR paragraph: "HIR is the resolved, typed, generic-aware semantic representation.
  It uses canonical declaration and type IDs, normalized function contracts and rows, core
  semantic operations, and source provenance while retaining type and contract-row parameters."
- Same ticket, snapshot paragraph: "Syntax, declaration, HIR, and semantic entity IDs key
  separate fact tables rather than successively rewritten annotated trees." And: "an unknown fact
  must never masquerade as a valid empty contract, resolved declaration, or concrete type."
