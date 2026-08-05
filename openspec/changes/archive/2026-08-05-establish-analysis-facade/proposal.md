## Why

The inspector currently imports compiler phase internals directly (`SemanticAnalysis.Result`
into the flow model). The pinned pipeline promises a supported analysis facade so tooling can
grow without reimplementing Silk semantics — and the realignment's definition of done makes the
facade the enforcement point for "every step is visualizable": a phase without facade-reachable
facts cannot ship. Formalizing the facade immediately after HIR lands keeps that rule from
becoming an afterthought.

## What Changes

- Introduce the analysis facade as the supported query surface over the immutable analysis
  snapshot: sources, syntax, declarations, references, types, contracts, and diagnostics —
  without exposing raw HIR storage. Ownership facts join when that phase lands.
- Migrate the inspector flow model and every lab to consume the facade exclusively; direct
  imports of phase internals from `apps/docs` are removed and disallowed.
- Facade results carry the same explicit recovery states as the underlying tables — damaged
  bodies leave unrelated declarations fully queryable.
- Document the rule in the compiler package: the facade is the only supported consumer surface;
  bootstrap need not implement every future editor query, but identities, recovery states, and
  provenance must let the facade grow without a second implementation of Silk semantics.

## Capabilities

### New Capabilities

- `bootstrap-analysis-facade`: The supported, snapshot-backed query surface for tooling, and the
  inspector's exclusive dependency on it.

### Modified Capabilities

- `bootstrap-syntax-inspector`: All labs and the flow model consume the facade only.

## Impact

The inspector flow model's imports and any lab reaching into phase internals; adds the facade
module to the compiler package's public API. No compiler phase behavior changes. May be folded
into `elaborate-bodies-to-hir` if the surface turns out thin — kept separate so the facade-only
rule is reviewed on its own.

## Plan References

- [Roadmap — Track 2, proposal 6, and "Definition of done"](../../../roadmaps/compiler-realignment.md)
- [Issue 06](../../../wayfinder/bootstrap-language/issues/06-bootstrap-compiler-pipeline.md),
  snapshot paragraph: "A supported analysis facade exposes queries over sources, syntax,
  declarations, references, types, contracts, ownership facts, and diagnostics without exposing
  raw HIR storage. Bootstrap need not implement every future editor query, but its identities,
  recovery states, provenance, and phase boundaries must allow the facade to grow without
  reimplementing Silk semantics in a separate tool."
- [Research note — Rust tooling resilience](../../../wayfinder/bootstrap-language/research/rust-tooling-resilience.md):
  the incomplete-program and public-analysis boundaries that motivated the facade.
