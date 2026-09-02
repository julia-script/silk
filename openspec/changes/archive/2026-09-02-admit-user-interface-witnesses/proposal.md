## Why

An interface conformance was admitted only when its mapping target was a two-segment `Intrinsic.*`
path; every other target was rejected with `is incompatible with`. The consequence is larger than
the error message suggests: **no user-defined type could conform to any interface at all.** Every
type that could witness `Order`, `Integer`, or any future interface was one of the built-in
scalars.

An interface whose only possible implementors are the twelve scalars is not really an interface.
`Order` could only order scalars, so — since every scalar is `Copy` and its equality is identity —
**sort stability was unobservable and a move-only element type could not exist**. `HashKey` (#34)
could only ever key a map by a scalar.

The damage lands in the test suites. PR #126 (`Vector.sort`) could not write two of its own
acceptance criteria — "equal elements keep their input order" and "a sort over a move-only element
type with no leak" — because no conforming element type could witness either. The implementation
met both, and nothing proved it. That is a gap that reads as coverage until someone checks, which
is the same shape as PR #84's decorative move-only assertions that hid the #86 Wasm bug.

## What Changes

- Admit a second witness target: a two-segment path naming one function of the provider's own
  actor, checked against the same substituted contract the `Intrinsic.*` target is checked against.
- Define the source witness's operand form. An interface operation never consumes its operands — a
  builtin operator does not, and a generic body is checked with its parameter non-`Copy` — so a
  source witness receives each contract operand by shared borrow and returns the contract result by
  value. A by-value operand would consume what the operator only reads and is rejected.
- Redirect specialization: a bound operator whose specialized operand type has a source-declared
  witness lowers to a call to that function, and instance discovery follows the conformance to it.
  A scalar argument keeps selecting its compiler-known operation, unchanged.
- License the operand read the redirected call needs: a place read whose value is never accessed as
  an owner and is only borrowed shared observes a non-`Copy` place without claiming it, exactly as
  the existing shared match projection does.
- Write PR #126's two unmet criteria, now that they are writable: sort stability over a user type
  with distinguishable-but-equal elements, and a sort over a move-only element type asserting
  acquires equal releases.

## Capabilities

### Modified Capabilities

- `bootstrap-declaration-index`: record what may serve as an interface witness target — one sealed
  intrinsic or one function of the provider's own actor — and the operand form each takes.
- `bootstrap-type-generics`: specialize a bounded parameter at a user type and reach that type's
  mapped function, while a scalar keeps reaching its compiler-known operation.
- `bootstrap-owned-sequence`: ordering is witnessed by user types, so stability is observable and a
  move-only element type can be ordered.

## Impact

The change affects conformance validation in the declaration index, the interface operation an
operator records during elaboration, instance discovery, lowering, and one MIR validation license.
It adds no diagnostic code, no runtime representation, no dynamic dispatch, and no instance-key
shape: the redirected operator becomes an ordinary static call to an ordinary Silk function.
`Integer` and every existing `Intrinsic.*` witness keep working with their source untouched.

It does not add blanket or conditional conformances (`impl<T: A> B for T`), more than one interface
per conformance, or a call surface for an interface operation whose name no operator spells (#118).
