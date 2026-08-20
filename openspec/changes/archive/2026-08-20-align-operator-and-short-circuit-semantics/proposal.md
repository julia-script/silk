## Why

Operator dispatch still depends on compiler-known operation names, and short-circuit expressions use a special impurity rejection pass. The confirmed language uses explicitly marked interface operations and analyzes the right side of `&&` or `||` as an ordinary conditionally executed branch.

## What Changes

- Add the confirmed operator marker to interface operations and resolve operators through ordinary static conformance for concrete and generic operands.
- Delete compiler-name privilege for `add`, `multiply`, and other operation spellings.
- Support distinct operand and result types allowed by the declared operation, including future vector-scalar and matrix operations.
- Remove the short-circuit impurity pass; analyze right operands with path-local ownership, Effect, cleanup, and type rules.
- Preserve left-to-right evaluation, boolean typing, conditional execution, explicit conversion, and deterministic diagnostics.

## Capabilities

### Modified Capabilities

- `bootstrap-operator-semantics`: select explicitly declared operator operations and ordinary conditional branch analysis.
- `bootstrap-complete-interface-contracts`: validate operator markers and operation signatures.
- `bootstrap-type-generics`: specialize operator witnesses through the ordinary bound path.
- `bootstrap-ownership`: compute path-local moves, loans, and cleanup for short-circuit branches.
- `bootstrap-hir`: record selected operation evidence and conditional right regions.
- `bootstrap-mir`: lower deterministic dispatch and short-circuit control flow.

## Impact

Depends on `unify-interface-service-conformance` and `generalize-borrows-and-callable-lifetimes`. It changes syntax facts for declarations, conformance checking, operator elaboration, ownership, HIR/MIR, evaluator, backends, diagnostics, and tests. It adds no dynamic operator lookup or runtime overloading.
