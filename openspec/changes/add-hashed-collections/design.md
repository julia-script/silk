## Context

Hashed collections are the first standard-library type whose behavior is supplied by the *user* of
the type rather than by the collection. `Vector` needs nothing from its element; a `HashMap` cannot
place a key without asking the key's own type where it goes. That question is a conformance, and
the conformance machinery — multi-operation bounds (#103, PR #107), a call surface for operations no
operator spells (#118, PR #141), and user-defined witnesses (#129, PR #142) — landed for exactly
this purpose.

#34's implementation note asks for that machinery to be confirmed against the `HashKey` shape before
any collection code is written. It was, by test, and the result changes what this change can do
next. The confirmation lives in `packages/compiler/test/HashKeyBoundForm.test.ts`.

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
actor. Four combinations, each tested rather than assumed:

| | equivalence, through `==` | hash, through `HashKey.hash(…)` |
| --- | --- | --- |
| scalar key, intrinsic witness | works | works |
| user key, source witness | works | **does not lower** |

What works is substantial and is what requirement 3 asked for. Multi-operation bounds are accepted,
both halves are reachable from one generic body, each specialization reaches its own witness, and
the contract shape this change wants — `fn hash(value: T, seed: HashSeed) -> u64`, whose second
operand is a fixed type rather than the interface's own parameter — is admitted by the conformance
check, with every operand forced through a shared borrow.

The one combination that fails is the one `HashKey.hash` needs. `Bound.operation(args)` lowers by
reading the intrinsic the witness names; a source witness is read only from the operator path. The
two capabilities landed independently and neither wired the other's case, so a non-operator
operation over a user-defined provider is admitted by analysis and then lowers to nothing: the
specialized instance fails MIR validation and no diagnostic is reported.

PR #141's design recorded the seam in advance — *"when witness admissibility widens, the node does
not change; only what lowering finds at the end of the conformance does"* — and admissibility
widened in PR #142 without lowering being taught to look.

The gap is not confined to user-defined keys, which is why it blocks the whole change rather than
one acceptance criterion. A scalar provider's conformance admits a sealed intrinsic and nothing
else, so a scalar key's hash cannot be ordinary Silk either. No intrinsic computes a hash, and
requirement 9 forbids one being added. Every key type therefore needs the source-witness path:
scalars need it to have a hash at all, and user types need it to have one that is reachable.

Two properties of the failure make it worth reporting as its own defect regardless of this change: a
program that passes analysis produces invalid MIR, and it does so silently. Whatever answers it
should either lower the call or report it.

## Decisions

The decisions below are independent of how the gap is closed. They are settled here so that
implementation can start the moment it is, rather than re-deriving the surface then.

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

The change is blocked on an enabling gap it does not own. Following this repository's established
pattern — #103, #118, and #129 were each split out as their own prerequisite rather than absorbed
into the ticket that needed them — the fix belongs to its own ticket, and the charter above holds
whichever way it is closed.

The gap's fix is well-localized: the bound-operation lowering needs the fallback the operator path
already has, and instance discovery needs to walk the same conformance. Neither adds a hash
operation, so requirement 9 is unaffected by it.
