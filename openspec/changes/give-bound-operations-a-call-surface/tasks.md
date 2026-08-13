## 1. Resolution

- [x] 1.1 Add a `ResolvedBoundOperation` call reference carrying the capability over the bounded
      parameter, the operation name, the interface's declaration of it, and the contract that
      operation declares over that parameter.
- [x] 1.2 Resolve a two-segment receiver naming an interface against the bounds of the declaration
      being elaborated, matching on the bound's recorded canonical capability, and take the spelling
      only for a member the bound's contract declares.
- [x] 1.3 Report a receiver naming an interface that bounds more than one of the declaration's type
      parameters, naming the parameters it is ambiguous across (`SEM0097`).
- [x] 1.4 Check the call against the interface's contract exactly as a compiler-known operation's is
      checked, and reject explicit type arguments on it.

## 2. Specialization

- [x] 2.1 Add `interfaceOperationIntrinsic`, selecting the compiler-known operation one provider's
      interface conformance maps for one operation.
- [x] 2.2 Add a `BoundOperationCall` HIR expression recording the capability, the bounded parameter,
      and the operation name, with no operation code and no intrinsic identity.
- [x] 2.3 Lower it by reading the witness the specialization admitted and continuing as the builtin
      call that witness names.
- [x] 2.4 Read the same witness when collecting reachable intrinsics, so target availability sees
      each specialization's own operation.

## 3. Acceptance

- [x] 3.1 Add a test declaring an interface with a non-operator-spelled operation, bounding a
      generic by it, calling that operation, and evaluating end to end.
- [x] 3.2 Add a test showing one body reaching two different witnesses for two different type
      arguments, with instructions that are not each other's width-neutral form.
- [x] 3.3 Add a test for a bound operation whose declared result is not the bounded parameter.
- [x] 3.4 Add tests for the collision case: the bound preferred inside a bounded body, the module
      function kept outside one, and the two-parameter receiver reported.
- [x] 3.5 Keep the operator-spelled operations and `Integer` passing unchanged.
