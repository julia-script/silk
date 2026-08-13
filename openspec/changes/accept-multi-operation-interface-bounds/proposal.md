## Why

A generic bound is written `T: Integer` and, until now, that spelling was all the compiler kept:
`TypeParameterFact` declared `readonly bound?: string`. Two consequences follow from a bare
spelling, and neither is visible while the only shipped interface — `Integer` (`numeric.silk`) —
declares one operation and is only ever bounded inside its own module.

- The spelling was matched against interfaces of **one** module: the module declaring the bounded
  function. A bound could therefore never name an interface another module declares, so
  `import silk.numeric { Integer }` followed by `fn twice<T: Integer>` was rejected as an unknown
  interface constraint. Only `silk/numeric` could bound anything by `Integer`.
- A witness was admitted by existence, not by coverage. `conforms` counted the conformance
  declarations matching one capability and provider and never looked at their operations, so a
  conformance that maps some of an interface's operations satisfied specialization exactly as well
  as one that maps all of them. With one operation there is no difference to see.

`HashKey` (#34) and `Order` (#36) both need a bound with more than one operation, and both are
blocked on those two properties.

## What Changes

- Replace the bare-spelling bound with a `BoundFact` that starts as the retained spelling and its
  syntax and becomes `ResolvedBound` during header completion, carrying the canonical identity of
  the interface it names and that interface's ordered operation contract.
- Resolve the bound in the bounded declaration's own module scope, so a bound may name any
  interface that declaration can see, including one another module declares.
- Check the witness completely at specialization: a type argument is admitted only when its
  conformance maps every operation the bound declares, and each unmapped operation is reported by
  name.
- Read the bound's recorded contract when deciding whether an operator on a bound-typed operand is
  the bound's own operation, and give that operator the operation's declared result type rather
  than the parameter — so a bound may declare `lessThan` and have it result in `bool`.

## Capabilities

### Modified Capabilities

- `bootstrap-type-generics`: record a bound as its resolved interface contract rather than a bare
  spelling, admit a bound that names an interface from another module, make every operation of a
  bound callable in the generic body at that operation's declared contract, and require a complete
  witness at specialization.

## Impact

The change affects the declaration index's type-parameter fact and header completion, and
elaboration's constraint checking and operator selection. It is type-system and resolution work
only: no runtime representation changes, no instance key or MIR shape changes, and the three
engines are unaffected except through which specializations analysis admits. `Integer` keeps
working as the single-operation case with no change to its source.

It does not add `HashKey` or `Order`, more than one bound on one parameter (`T: A + B`), any
dynamic dispatch or vtable, or a call surface for an interface operation whose name is not spelled
by an operator.
