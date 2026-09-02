## 1. Bound Form

- [x] 1.1 Replace `TypeParameterFact.bound`'s bare spelling with a `BoundFact` retaining the
      spelling and its one-segment type path, and collect it only from a real bound type path
      instead of falling back to the parameter's own name during recovery.
- [x] 1.2 Resolve every declaration's bounds during header completion, in that declaration's own
      module scope, recording the named interface's canonical identity and ordered operation names.
- [x] 1.3 Drop the resolver's own diagnostics for a bound so an unresolved bound keeps being
      reported at the specialization that needs it, not at the declaration.

## 2. Constraint Checking

- [x] 2.1 Build the specialization's capability from the bound's recorded canonical identity rather
      than searching the declaring module's interfaces by spelling.
- [x] 2.2 Add `unmappedInterfaceOperations` returning, in declaration order, the interface
      operations one provider's selected conformance leaves unmapped.
- [x] 2.3 Report each unmapped operation by name at the specialization, alongside the existing
      no-witness and unknown-constraint reports.

## 3. Operator Selection

- [x] 3.1 Decide an operator on a bound-typed operand from the bound's recorded operation names.
- [x] 3.2 Substitute the bounded parameter only where the compiler-known operation carries its
      actor's own type, so a bound comparison keeps its `bool` result.

## 4. Acceptance

- [x] 4.1 Add tests declaring a two-operation interface, bounding a generic by it, calling both
      operations, and evaluating the result — at one provider and at two.
- [x] 4.2 Add a test asserting a type argument whose conformance omits one operation is rejected
      with a diagnostic naming that operation.
- [x] 4.3 Add tests for a bound naming an imported interface, for a bound comparison's `bool`
      result, and for the recorded bound contract on the declaration index.
- [x] 4.4 Keep `Integer` working unchanged as the single-operation case, with its source untouched.
