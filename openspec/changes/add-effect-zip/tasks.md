## 1. Combinators

- [x] 1.1 Add `Pair<A, B>` and `Triple<A, B, C>` to canonical `silk/effects` source with public
      fields, so a caller in another module can project the collected values.
- [x] 1.2 Add `Effect.zip` as ordinary Silk: run each operand with a plain `run` statement in
      declaration order, so short-circuiting follows from the propagation exit rather than from a
      failure test, and the unrun operand is released by ordinary local cleanup.
- [x] 1.3 Add `Effect.zip3` with the same shape over three operands, spelled out rather than derived
      from nested `zip`, so the caller's projection does not depend on how it was built.
- [x] 1.4 Union every operand's failure row and requirement row and add nothing of the combinator's
      own.
- [x] 1.5 Regenerate the compiler-shipped standard-library source table.

## 2. Acceptance

- [x] 2.1 Assert `zip` runs its two operands in declaration order, on an allocation trace whose
      per-operand event counts differ so the order is unambiguous in either direction.
- [x] 2.2 Assert a first-operand failure stops `zip` without running the second operand, and that
      the failure's own payload reaches the caller.
- [x] 2.3 Assert a second-operand failure propagates unchanged with no pair constructed.
- [x] 2.4 Assert `zip3` runs all three operands in declaration order and stops at a middle failure
      without running the third.
- [x] 2.5 Assert acquires equal releases on every path, so an unrun operand is neither stranded nor
      released twice.
- [x] 2.6 Assert both row unions on `zip` and on `zip3` through the encoded call types.
- [x] 2.7 Assert both combinators resolve to shipped Silk declarations and that neither name appears
      in the intrinsic catalog.
- [x] 2.8 Assert the piped form resolves to the same declaration with the same result.
- [x] 2.9 Assert three-engine parity — evaluator, LLVM, and Wasm — on an allocation-free program.

## 3. Documentation

- [x] 3.1 Regenerate the standard-library documentation page.
- [x] 3.2 Add `zip` and `zip3` to the language reference's list of library-defined combinators, and
      record why arity is fixed and why `Effect.all` over a collection is not available.
