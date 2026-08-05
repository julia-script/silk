## Why

Nothing in the spike checks ownership, and — decisively — nothing produces the cleanup plan that
MIR lowering will consume to insert drops. In the pinned design the ownership phase is a
producer, not just a gate: deterministic reclamation with no manual `free` means *something* must
decide where cleanup happens, and that something is this phase. Landing it on the frozen grammar
slice (where it is trivially satisfiable) establishes the phase, its fact table, and the
cleanup-plan artifact before lowering exists to need them.

## What Changes

- Add the ownership phase over typed generic HIR, run once per declaration: ownership, lexical
  borrowing, mutation, complete initialization, named-scope outlives and escape rules, and live
  owners at structured exits — per issue 01's resolved semantics.
- Publish ownership and scope facts as an immutable table in the analysis snapshot, exposed
  through the analysis facade.
- Produce the target-neutral cleanup plan as a first-class artifact with a deterministic textual
  encoding; it inserts no target-specific drops.
- Add an inspector lab: per-binding scope/lifetime timeline over source spans, and the cleanup
  plan as an ordered list per structured exit path.
- Resolve the open scope question recorded in the roadmap: whether to pull forward minimal
  bindings/moves from issue 08 so the checker is non-vacuous, or keep the slice frozen and accept
  a trivially-satisfiable phase. Default position: stay frozen; the phase's artifacts, not its
  verdicts, are what this proposal delivers.

## Capabilities

### New Capabilities

- `bootstrap-ownership`: The ownership/scope analysis over generic HIR, its fact table, and the
  target-neutral cleanup-plan artifact.

### Modified Capabilities

- `bootstrap-analysis-facade`: Ownership facts become queryable.
- `bootstrap-syntax-inspector`: Scope/lifetime timeline and cleanup-plan labs.

## Impact

New phase between elaboration and instance discovery; no existing behavior changes on the frozen
slice (every value is a copyable `i32` parameter or temporary). If the open question resolves
toward minimal bindings, grammar and elaboration are also touched — that decision gates the
proposal's final scope.

## Plan References

- [Roadmap — Track 3, proposal 7, and "Foundation vs. features"](../../../roadmaps/compiler-realignment.md)
- [Issue 06](../../../wayfinder/bootstrap-language/issues/06-bootstrap-compiler-pipeline.md),
  frontend checking order, step 4: "Check ownership, lexical borrowing, mutation, complete
  initialization, named-scope outlives and escape rules, and live owners at structured exits once
  on typed generic HIR. This produces ownership facts and a target-neutral cleanup plan; it does
  not insert target-specific drops."
- [Issue 01 — Ownership, lifetimes, and scoped allocation](../../../wayfinder/bootstrap-language/issues/01-ownership-lifetimes-and-scoped-allocation.md):
  the resolved semantics this phase enforces — affine single ownership, lexical non-escaping
  borrows, named `Scope<S>` outlives hierarchy, and typed-infallible cleanup in last-acquired,
  first-released order.
- [Issue 08](../../../wayfinder/bootstrap-language/issues/08-prototype-bootstrap-syntax.md):
  owns any binding/move spelling if the open question resolves toward pulling syntax forward.
