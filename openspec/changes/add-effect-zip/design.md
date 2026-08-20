## Context

Collecting several Effect results is a single idea with two possible shapes: a fixed number of
distinct parameters, or one collection parameter. The two look interchangeable from the source side
and are not interchangeable underneath, so the choice had to be settled before the combinator could
be written. This document records why fixed arity is the shape the existing machinery supports and
why the collection shape is a compiler-core project rather than a longer function body.

## Goals / Non-Goals

Goals:

- One combinator that runs Effects in order and keeps every success value.
- Short-circuiting that follows from how the body is written rather than from a comment asserting
  it.
- Row unions that widen exactly as composition already widens them.

Non-Goals: parallelism, `race`, `timeout`, error accumulation, and `Effect.all` over a collection.

## Decisions

### Arity is fixed, and each operand is a distinct parameter

An Effect value is compiler-private: `Layout.ts` answers an unavailable layout for it with the
detail that compiler-private effect values have no target layout, and `Type.ts` states that Effect
values never cross the executable ABI. That is workable everywhere an Effect is passed, returned, or
bound, because hidden-identity monomorphization (`EffectIdentityArgument`) erases the value before
lowering — the specialized target never has a runtime Effect at all.

Erasure needs the identity to be statically known at each use. A distinct parameter has one. An
element of a `Vector` does not: `RawBuffer` contents are runtime-indexed storage, so the element's
identity is a runtime value by construction and there is nothing for specialization to erase. The
two shapes therefore differ in kind, not in degree.

This is not a prediction. `Vector<Effect<i32>>` built with `make` and `append` produces **zero**
semantic diagnostics today and then fails MIR verification with `MissingTypeLayout: function
references nominal:silk/vector.Vector<effect:Shared<builtin:i32!?>> without a layout entry`, plus
`InvalidCallShape` violations inside `silk/vector`. The evaluator reports `Blocked/InvalidMir` and
the Wasm backend refuses to emit. Choosing fixed arity keeps this combinator away from a wall that
has no diagnostic in front of it.

`zip3` is spelled out rather than derived as `zip(zip(a, b), c)`. The derived form returns
`Pair<Pair<A, B>, C>`, which makes the caller's projection depend on how the combinator was built
rather than on how many Effects they combined.

### Ordering and short-circuiting are structural

The body is three statements:

```silk
let first = run self
let second = run other
return Pair<A, B> { first: move first, second: move second }
```

`run self` is what executes the first Effect, and it is sequenced before `run other` by ordinary
statement order. A typed failure at the first `run` propagates out of the body immediately, so the
second `run` is never reached — the combinator does not test for failure or choose to stop, it
simply never gets there. The unrun `other` is an owned local of the frame the propagation leaves,
and the propagation exit releases it exactly as it releases every other local.

This is why the combinator needs no reification. `ensuring` and `provideEffect` reify with
`Effect.result` because they have work to do *after* a failure and must not do it on the propagation
path. `zip` has nothing to do after a failure, so the propagation path is the correct one.

### The collected values are ordinary public data

`Pair` and `Triple` are plain structs with `pub` fields. Non-public fields would compile — the
combinator constructs them inside their own defining module — and then be unreadable by every
caller, since cross-module projection of a non-public field raises `SEM0028`.

Cross-module *construction* stays closed regardless: `SEM0021` limits raw construction to the
defining module, so a caller reads a `Pair` that `zip` produced but cannot build one. That is the
same shape `Result` already has, where `succeed` and `failResult` are the constructors, and it costs
nothing here because the only source of a `Pair` that matters is `zip` itself.

## Risks / Trade-offs

Fixed arity does not cover a count known only at runtime. That is the case `Effect.all` exists for,
and it stays uncovered — deliberately, since the alternative is a combinator that type-checks and
then fails to lower. A caller combining more than three Effects nests: `zip(zip3(a, b, c), d)`.

Adding `zip4` and beyond is mechanical if a real program needs it. Nothing about this change decides
that question either way.

## Migration Plan

None. This adds declarations; no existing source changes meaning.

## Open Questions

Whether a storable Effect representation is worth building for `Effect.all` is left open, to be
raised by a program that actually needs a runtime-sized collection of Effects rather than filed
speculatively. The prerequisite is recorded here so that issue starts from the real cost.
