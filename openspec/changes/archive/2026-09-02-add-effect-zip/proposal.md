## Why

`silk/effect` has no combinator that runs several Effects and keeps every result. A caller who
needs two success values writes a `flatMap` whose callback closes over the first value and returns
the second Effect, then builds the pair by hand inside that callback — the combination is spelled
out at every call site, and the closed combinator list in `bootstrap-flow-functions` omits `zip`
without anything rejecting it.

Sequential collection needs no concurrency, so this is an ordinary Silk addition: run the first
Effect, run the second, and hold both values. The one design question is arity, and it is not a
matter of taste. A collection-taking `all` would need `Vector<Effect<...>>`, which the language
cannot express: Effect values are compiler-private and have no target layout, while `RawBuffer`
contents are runtime-indexed storage. That combination is accepted by semantic analysis today and
then dies at MIR verification with `MissingTypeLayout`, so it is a silent-miscompile shape rather
than a clean rejection. Fixed arity avoids the question entirely — with each operand a distinct
parameter, no Effect is ever stored, and hidden-identity specialization erases every one of them
before lowering.

## What Changes

- Add `Effect.zip(self, other)` to canonical standard-library source. It runs the two Effects in
  declaration order, stops at the first typed failure without running the second, and returns a
  `Pair` holding both success values.
- Add `Effect.zip3(self, second, third)` with the same semantics over three operands, so extending
  arity means adding a parameter rather than accepting a collection.
- Add `Pair<A, B>` and `Triple<A, B, C>` as ordinary public data with public fields, so a caller in
  another module can project the collected values.
- Union both the failure rows and the requirement rows of every operand: `! E | F ? R | S` for
  `zip`, `! E | F | G ? R | S | T` for `zip3`.
- Add `zip` and `zip3` to the closed list of combinators that resolve to ordinary Silk declarations,
  so no compiler-side name recognition is introduced.

Sequencing is the body's own statement order rather than a promise the combinator has to keep. The
second operand is a value here and nothing runs it until the first `run` has produced a success, so
short-circuiting is structural: a typed failure propagates out of the body before the second `run`
is reached, and the unrun operand is released by the ordinary local cleanup of the frame the
propagation leaves.

## Capabilities

### Modified Capabilities

- `bootstrap-flow-functions`: specify the sequential collecting combinator — that it runs its
  operands in declaration order, collects every success value into ordinary data, stops at the first
  typed failure without running the later operands, and unions every failure and requirement row —
  and add `zip` and `zip3` to the closed list of library-defined combinators.

## Impact

The change affects canonical standard-library source (`silk/effect`), the compiler-shipped source
table generated from it, the generated standard-library documentation page, the language reference's
combinator list, and acceptance tests. It adds no intrinsic, no HIR or MIR operation, no diagnostic
code, and no compiler-side name recognition.

It introduces no parallelism, no `race` or `timeout` (both still need concurrency and stay absent),
and no error accumulation — the first typed failure stops the sequence.

## Out of scope

`Effect.all` over a collection is not part of this change and is not deferred pending a smaller fix.
It requires `Vector<Effect<...>>`, so its prerequisite is giving Effect values a storable runtime
representation — a compiler-core change, not a library addition. Two further questions would remain
open even then: collecting into a `Vector` forces `! OutOfMemoryError ? &mut Allocator` onto the result,
which no requirement in this capability authorizes, and moving an affine `once Effect` out of a
`Vector` has no operation today (`Vector.get` is a bitwise read documented for `Copy` elements).
