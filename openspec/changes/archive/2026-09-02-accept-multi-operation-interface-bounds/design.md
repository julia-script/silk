## Context

Bounds are monomorphization-time only. An interface selects compiler-known operations during
specialization; it creates no effect requirement, no provider slot, and no runtime dispatch
(`numeric.silk`). That property is preserved here in full — the change is about what the compiler
records for a bound and what it checks before substituting a type argument, not about what reaches
the engines.

## Goals / Non-Goals

Goals:

- One bound may name an interface declaring any number of operations.
- Every operation the bound declares is usable in the generic body, at the contract the interface
  declares for it.
- A type argument is admitted only against a witness that covers the bound's whole contract, and a
  rejection names the operation that is missing.
- A bound may name an interface another module declares.

Non-Goals: `HashKey` and `Order` themselves, more than one bound on one parameter, dynamic
dispatch, and any runtime representation change.

## Decisions

### The bound is a resolved contract, not a spelling

`TypeParameterFact.bound` becomes

```ts
export type BoundFact =
  | {
      readonly _tag: 'ResolvedBound'
      readonly spelling: string
      readonly path: TypePathFact
      readonly capability: CanonicalId
      readonly operations: ReadonlyArray<string>
    }
  | { readonly _tag: 'UnresolvedBound'; readonly spelling: string; readonly path: TypePathFact }
```

Collection keeps the syntax it can see: the spelling and its one-segment type path. Header
completion — the pass that already resolves declared types, conformance capabilities, and rows
through each module's own resolver — resolves that path and, when it names an interface, records
the interface's canonical identity together with its ordered operation names.

Two consumers then read one fact instead of re-deriving it. Constraint checking builds the
capability from `capability` rather than searching one module's interfaces by spelling, which is
what admits a bound on an interface another module declares. Operator selection asks
`operations` whether the bound declares the operation the operator spells, instead of repeating
the same search.

Recording the operation names on the bound, rather than only the interface identity, is what makes
the fact answer the question the bound exists to answer: which operations this parameter offers.

### An unresolved bound is still reported at the specialization

Header completion deliberately drops the resolver's own diagnostics for a bound. A bound that names
nothing, or names a declaration that is not an interface, stays `UnresolvedBound` and is reported
where it was reported before: at the call that would have had to satisfy it, where the type
argument is known. A declaration whose bogus bound no call ever specializes keeps producing no
diagnostic, exactly as before.

### Existence is not coverage

`conforms` answers whether a provider has a witness. For an interface capability that meant
counting conformance declarations, with no attention to their operations. Specialization now asks a
second question through `unmappedInterfaceOperations`: which operations of the interface the
selected conformance leaves unmapped. Each one produces its own diagnostic naming
`Provider does not implement Bound.operation`, so a bound with two operations cannot be
half-satisfied by a witness that supplies one.

This is deliberately a separate question rather than a stricter `conforms`. `conforms` also answers
for `Drop` and `Report`, whose completeness rules are their own, and its existing callers ask only
about existence.

### An operator on a bound-typed operand takes the operation's contract

An operator whose operand is a bound parameter is elaborated against the compiler-known operation
for a stand-in actor, then generalized. Generalizing every position to the parameter is only
correct while every bound operation happens to result in the parameter, which is true of `add` and
false of `lessThan`. The rule is instead: substitute the parameter for the stand-in actor's own
type wherever it appears in the compiler-known contract, and leave every other position alone.
`add` gives `(T, T) -> T`; `lessThan` gives `(T, T) -> bool` — in both cases exactly the contract
the interface declares.

### What a generic body can call

An operation whose name an operator spells — `add`, `subtract`, `lessThan`, `equals` — is called
through that operator, and every such operation of the bound is callable. An interface operation
whose name no operator spells has no call surface in a generic body: interfaces are not actors, and
`Bound.operation(value)` resolves to a public function of the module declaring `Bound`, not to the
bound's operation.

That surface is left to the change that needs it. `HashKey` (#34) will need one for `hash`, and
choosing its spelling is a language decision — the parallel with services suggests
`Bound.operation(args)`, and unlike an operator it must consult the witness's selected operation at
specialization rather than reusing the width-neutral scalar lowering an operator already has.
Nothing here forecloses it.
