## 1. Witness Admissibility

- [x] 1.1 Accept a `<Provider>.<function>` mapping target in the interface conformance branch of the
      declaration index, resolving the function in the provider type's own module.
- [x] 1.2 Check the source witness against the substituted contract: same arity, each operand a
      shared borrow of the contract's operand type, the contract's result by value, ordinary
      function kind, no type parameters, no failure row, and no requirement row.
- [x] 1.3 Report a mapping whose provider-qualified name matches no declaration as a mapped
      operation that does not exist, and keep the `Intrinsic.*` branch's checks unchanged.
- [x] 1.4 Add `interfaceWitnessImplementation`, selecting the provider function one conformance maps
      an operation to and returning nothing for an intrinsic target.

## 2. Specialization And Dispatch

- [x] 2.1 Record the bound interface operation an operator spells — capability, provider parameter,
      and operation name — on the operator fact and on the `BuiltinCall` it produces.
- [x] 2.2 Follow the conformance during instance discovery so a source witness is reachable even
      though no ordinary call names it.
- [x] 2.3 Lower a redirected operator to a shared borrow of each operand local and one ordinary
      static call, leaving an intrinsic-mapped operator on its compiler-known operation.
- [x] 2.4 License a place read that is never accessed as an owner and is borrowed shared to observe
      a non-`Copy` place, as the shared match projection already is.

## 3. Acceptance

- [x] 3.1 Add tests declaring a two-operation interface, witnessing it with a user struct, and
      specializing a generic bounded by it at that struct so both operations are reached.
- [x] 3.2 Add tests rejecting an incomplete conformance by operation name, a by-value operand, a
      disagreeing result, and a mapping to a function the provider does not declare.
- [x] 3.3 Add a test specializing one bound at both a scalar and a user type in one program.
- [x] 3.4 Write PR #126's first unmet criterion: sort stability over a user element type whose equal
      elements stay distinguishable.
- [x] 3.5 Write PR #126's second unmet criterion: a sort over a move-only element type asserting the
      acquire and release counts agree.
- [x] 3.6 Keep `Integer`, `Order`, and every shipped `Intrinsic.*` witness passing unchanged.
