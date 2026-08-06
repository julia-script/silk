## Why

Silk can now build and mutate nominal compiler-shaped data, but it still cannot represent a value
whose runtime member is one of several closed nominal alternatives. Structural unions must be
normalized and laid out before exhaustive matching is added, so matching can consume one canonical,
target-aware representation instead of defining type identity, widening, ownership, and runtime tags
for itself.

## What Changes

- Add `A | B` union types as unordered, duplicate-free structural sets of canonical nominal members;
  flatten nested unions, erase spelling order and duplicates, and represent `Never` as the empty union.
- Add immediate-context union injection and widening without changing inference: an unannotated
  binding retains its precise initializer type, while a nominal or narrower union may enter a
  containing expected union explicitly in typed HIR.
- Reject non-nominal members, incompatible widening, stored borrows, and uses that would require
  retroactive widening or narrowing; narrowing remains reserved for `match-exhaustively`.
- Extend ownership, instance discovery, target-aware layout, HIR, MIR, evaluation, native emission,
  and WebAssembly emission with canonical union payloads, active-member cleanup, deterministic tag
  remapping, and compiler-planned discriminant/payload placement.
- Keep compiler-published relationships DAG-shaped: injection, widening, transport, and cleanup are
  explicit logical operations with canonical member mappings, while backend-private branches used to
  realize them never become compiler graph edges.
- Expose normalized types, contextual conversions, layouts, runtime values, cleanup, MIR operations,
  traces, and backend provenance through the immutable analysis facade and unified `/labs` workbench.
- Extend the three-engine differential and fresh-process determinism corpus with direct injection,
  nested/duplicate normalization, union widening, aggregate-contained unions, move-only cleanup, and
  invalid contextual-conversion cases.
- **BREAKING**: type identity, encoders, layout plans, calling shapes, HIR, ownership facts, MIR, traces,
  facade projections, and backend contracts gain canonical structural-union cases without preserving
  compatibility with the unreleased pre-union representations.

## Capabilities

### New Capabilities

- `bootstrap-structural-unions`: Canonical union type algebra, contextual injection and widening,
  precise inference boundaries, `Never`, ownership behavior, and observable runtime value semantics.

### Modified Capabilities

- `bootstrap-syntax`: Parse and recover union type expressions and `Never` losslessly in every
  supported type position.
- `bootstrap-semantic-facts`: Publish canonical union members, expected-context conversions, precise
  inferred types, and causal diagnostics without retroactive widening or narrowing.
- `bootstrap-hir`: Represent normalized union types, injection, and widening as canonical typed HIR.
- `bootstrap-ownership`: Derive union Copy/cleanup behavior from all members and clean exactly the
  active owned payload.
- `bootstrap-instances`: Discover union runtime types and their nominal member dependencies through
  canonical instance keys.
- `bootstrap-target-layout`: Compute deterministic compiler-owned discriminant, payload, alignment,
  and calling-shape facts for every concrete union.
- `bootstrap-mir`: Carry logical union types and verified injection/widening operations with canonical
  member/tag mappings inside the structured control DAG.
- `bootstrap-evaluation`: Evaluate immutable tagged union values, widening, transport, and active
  payload cleanup directly from MIR and its layout plan.
- `bootstrap-backend`: Realize the same union layout and tag mappings in native LLVM and direct
  WebAssembly emission without choosing layout or reconstructing semantics.
- `bootstrap-analysis-facade`: Expose immutable deterministic union type, conversion, ownership,
  layout, MIR, evaluation, and backend provenance queries.
- `bootstrap-syntax-inspector`: Add coordinated structural-union views and valid/invalid presets to
  the unified `/labs` workbench.
- `bootstrap-compiler-driver`: Require evaluator/native/WebAssembly parity and deterministic artifacts
  for the structural-union corpus.

## Impact

The compiler type model, parser, elaboration facts, HIR, ownership analysis, instance discovery,
layout planner, MIR verifier/encoder, lowering, evaluator, both backends, analysis facade, language
highlighting, `/labs`, goldens, and release-candidate consumer all gain union cases. The change adds no
external dependency and does not include `match`, pattern narrowing, exhaustiveness, generic union
aliases, failure-row lowering, niche optimization, public ABI/serialization tags, or automatic layout
optimization.
