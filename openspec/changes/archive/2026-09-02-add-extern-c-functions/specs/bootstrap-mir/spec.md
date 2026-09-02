## ADDED Requirements

### Requirement: Foreign calls are explicit verified MIR operations

A call to a foreign function SHALL lower to one dedicated MIR operation carrying the native symbol,
the ABI, the classified C signature, the argument locals, the destination local, the logical result
type, and provenance. The verifier SHALL reject an operation whose argument count or argument local
types disagree with its signature or whose destination type disagrees with the signature result.
The operation SHALL encode deterministically, and equal source SHALL produce byte-identical
encodings across processes.

#### Scenario: Lower a foreign call

- **WHEN** a reachable function calls `unsafe extern "C" fn silk_test_add(a: i32, b: i32) -> i32`
- **THEN** its MIR contains one foreign-call operation with symbol `silk_test_add`, ABI `C`, signature `(i32, i32) -> i32`, two argument locals, and an `i32` destination

#### Scenario: Verify an arity mismatch as data

- **WHEN** a constructed foreign-call operation supplies one argument to a two-parameter signature
- **THEN** verification reports one structural violation naming the operation and neither trap nor throw occurs
