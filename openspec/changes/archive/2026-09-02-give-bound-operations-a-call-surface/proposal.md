## Why

A bound may declare any number of operations, and every one of them is checked for coverage before
a type argument is admitted. Only some of them are callable. An operation whose name an operator
spells — `add`, `subtract`, `lessThan`, `equals` — is reached through that operator; an operation
whose name no operator spells is reachable from nowhere. `Bound.operation(value)` parses today, but
it resolves to a public function of the module declaring the interface, which is why
`Integer.add(40, 2)` resolves at all.

That gap blocks `HashKey` (#34), whose whole contract is `hash`. A generic bounded by `HashKey`
could reach the equivalence half through `equals` and could not reach the hash half at all.

## What Changes

- Give a bound's operations a call surface spelled `Bound.operation(args)`, qualified through the
  bound's own name, parallel to a service operation.
- Prefer the bound over the interface's declaring module when the receiver names a bound of the
  declaration being elaborated and the member is a name that bound's contract declares. Every other
  member of that module keeps resolving where it resolved before, and a body with no such bound is
  untouched.
- Report a receiver that names an interface bounding two of one declaration's parameters, where no
  single parameter answers the call.
- Select the operation's implementation from the witness at specialization rather than from the
  width-neutral scalar lowering an operator already carries, so two providers of one interface may
  answer one operation with two unrelated instructions.

## Capabilities

### Modified Capabilities

- `bootstrap-type-generics`: make every operation a bound declares callable in a generic body
  through the bound's own name, resolve that spelling to the bound rather than to a same-named
  public function of the interface's declaring module, and select the concrete operation from the
  witness the specialization admitted.

## Impact

The change affects call resolution and elaboration, one new HIR expression, lowering, and the
reachable-intrinsic report. Bounds stay monomorphization-time only: the call creates no effect
requirement, no provider slot, and no runtime dispatch, and it reaches the engines as the same
compiler-known operation an operator would have reached them as. Operator-spelled operations are
unchanged, and `Integer` keeps working with its source untouched.

It does not add `HashKey` or `Order`, does not widen which witnesses are admissible (#129), does
not make a bound operation a first-class value, and does not give a bound operation type arguments
of its own.
