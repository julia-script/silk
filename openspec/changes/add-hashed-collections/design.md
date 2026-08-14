## Context

Hashed collections are the first standard-library type whose behavior is supplied by the *user* of
the type rather than by the collection. `Vector` needs nothing from its element; a `HashMap` cannot
place a key without asking the key's own type where it goes. That question is a conformance, and
the conformance machinery — multi-operation bounds (#103, PR #107), a call surface for operations no
operator spells (#118, PR #141), and user-defined witnesses (#129, PR #142) — landed for exactly
this purpose.

#34's implementation note asks for that machinery to be confirmed against the `HashKey` shape before
any collection code is written. It was, by test, and the confirmation found a gap that had to close
first. The confirmation lives in `packages/compiler/test/HashKeyBoundForm.test.ts`.

## Goals / Non-Goals

Goals: a `HashKey` contract a user type can witness in ordinary Silk; `HashMap<K, V>` and
`HashSet<T>` over the owned-allocation substrate; equivalence and hash bound to each other by spec;
iteration order fixed by a seed; correct ownership of move-only keys and values; and no hash
operation anywhere in the compiler, the MIR, the evaluator, or a backend.

Non-Goals: an ordered map or set, a concurrent collection, an iteration protocol or any iterable
abstraction, the `Vector` sort (#36), and any change to how conformances are declared or checked.

## The confirmation, and the gap it found

`HashKey` needs two operations over one bound: an equivalence, whose name `==` spells, and a hash,
whose name no operator spells. A `HashMap` is generic over its key, so each half must work against
both witness kinds a provider may have — a sealed intrinsic and a function of the provider's own
actor. Four combinations, each tested rather than assumed. Three worked. The fourth — the
non-operator call over a user-defined provider, which is exactly what `HashKey.hash` is — passed
analysis and then lowered to nothing, because `Bound.operation(args)` read only the intrinsic a
witness named while a source witness was read only from the operator path.

The gap was not confined to user-defined keys, which is why it blocked the whole change rather than
one acceptance criterion. A scalar provider's conformance admits a sealed intrinsic and nothing
else, so a scalar key's hash could not be ordinary Silk either, and requirement 9 forbids adding an
intrinsic that hashes.

It was split out as its own prerequisite — #155, PR #157 — following what this repository did three
times before, and closed there: lowering gained the fallback the operator path already had, instance
discovery walks the same conformance, and a conformance selecting no lowerable witness now reports
`SEM0101` rather than dropping the call into invalid MIR. The confirmation's fourth case was
inverted to assert the working outcome and kept, because it is the path every `HashKey.hash` call
over a user-defined key takes.

## What the implementation had to work around

Two language facts shaped the collections more than any choice recorded below did.

**A bound is not carried into a nested generic call.** A `K: HashKey` body cannot hand its own `K` to
another `K: HashKey` function — the same constraint `vector.silk` records for its sort. Everything
that needs a witness has to be written in the body that needs it, so `insert`, `contains`, `indexOf`,
`get` and `remove` each carry their own probe loop, and `HashSet` is the table written out again
rather than a `HashMap` with empty values.

Storing each entry's hash alongside its key is what keeps this from spreading. Growth rehomes an
entry under the hash it was placed with rather than hashing it again, so migration, placement and
release are ordinary unbounded code that the five bounded operations can call.

**Scalar types cannot witness `HashKey`.** A conformance whose provider is a scalar admits a sealed
compiler operation and nothing else, and no compiler operation computes a hash — nor may one be
added. So a key built from an integer is a declared type. `Word` is the standard library's, and the
constraint is documented on it rather than left for a caller to discover.

## Decisions

The decisions below were settled while the gap was open, because none of them depended on how it
closed, and they stood unchanged once it did.

### The equivalence operation is named `equals`

#34's example names it `equivalent`. It is named `equals` instead, for the same reason `order.silk`
declares `lessThan` rather than a three-way `compare`: an operator-spelled name is reachable through
its operator, and `==` spells `equals`. That is not merely a convenience. It is the one half of
`HashKey` that works today for both witness kinds, confirmed by test, and naming it `equivalent`
would put both halves behind the same unlowerable call for no gain.

The name is also honest about the contract. A witness supplies one relation; the spec requires the
hash to agree with it. Calling it `equals` says the relation is equality of keys as this map
understands them, which is exactly what lookup asks.

### The seed is an operand of the hash, not part of the key

`HashSeed` is one value for the whole map, so it is a parameter of `hash` rather than state a key
carries. The contract is `fn hash(value: T, seed: HashSeed) -> u64`, and a witness receives both
operands by shared borrow — forced by the conformance rule, not chosen, and confirmed by test for
this exact shape including the non-parameter operand type.

Threading the seed through the operation, rather than storing it in the key or reading it from a
global, is what makes requirement 7 achievable in ordinary Silk: the map holds one seed, hands it to
every hash it computes, and two runs over one seed compute one set of hashes. Nothing consults an
allocation address, an insertion timestamp, or any ambient source of entropy, so the order is a
function of the seed and the insertion sequence alone.

### The result is `u64` regardless of the key

A fixed-width hash result keeps the bucket computation one shape for every key type and keeps the
contract checkable: the conformance check compares the witness's result against the contract's
substituted result, and a result that varied with the key would have to be constrained separately.
`u64` is wide enough that the map's own bucket reduction, not the witness, is where width is lost.

### A bucket index is reduced in u64 arithmetic, before it narrows

A hash is `u64` and a bucket index is `usize`, which is 64 bits natively and 32 under WebAssembly.
Narrowing the hash first and then reducing it would put a key in different buckets on different
engines, and the map would answer correctly everywhere while presenting its entries in two different
orders — a divergence no test that only checked lookups would catch. The remainder is therefore
taken against the width in `u64`, and only the result — already smaller than the bucket count —
narrows. Requirement 7 says *in every engine*, and this is the line where that is won or lost.

### Equivalence implies equal hash is a spec requirement, not documentation

Requirement 6 is stated in the capability spec as a requirement on witnesses, because a witness that
breaks it breaks lookup in a way the collection cannot detect, diagnose, or recover from: the key is
present, the probe visits the wrong bucket, and the map reports absence. The compiler cannot check
the implication, so the spec states it and the collection's tests demonstrate the consequence over a
witness that honors it.

### The collection owns what it stores

A `HashMap` that owns move-only keys and values releases each exactly once — on removal, on
overwrite of an existing key, and on the map's own drop, including the drop of a map left non-empty.
This is the acceptance criterion most easily skipped, and it is the one that decides whether the map
composes with the rest of the language, so it is specified as behavior rather than left to the
implementation. The mechanism is the substrate `Vector` already uses; nothing new is required of it.

## Risks / Trade-offs

The probe loop is written five times over in `hash_map.silk` and again in `hash_set.silk`, because
the bound cannot be forwarded. That is duplication a future change to probing has to find in every
copy, and it is the price of the constraint rather than a choice; if bounds ever forward, the copies
collapse into one.

A removed slot is a mark rather than a backward shift, so a map that is filled and emptied many
times without growing keeps probing through the marks. Growth clears them, and growth is triggered
by occupied *and* removed slots together, so the marks cannot accumulate without bound. The
alternative — shifting entries back on removal — moves owned keys and values on a path that has no
other reason to move them, and this change preferred the simpler ownership.

`get`, `keyAt`, `valueAt` and `elementAt` read a copy out of the collection and so answer only for
`Copy` keys and values, exactly as `Vector.get` does and for the same reason. A collection of
move-only values is looked up with `indexOf`, which names the bucket without moving anything, and
emptied with `remove`, which transfers ownership out.
