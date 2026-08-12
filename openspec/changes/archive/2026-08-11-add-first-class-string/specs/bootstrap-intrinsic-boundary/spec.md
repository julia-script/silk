## ADDED Requirements

### Requirement: String intrinsics expose only view primitives

The sealed `Intrinsic` namespace SHALL expose only the target-neutral primitives needed to preserve
the abstract `string` representation and implement its compiler-selected operators: unchecked
formation from a live UTF-8 byte view, immutable UTF-8 byte viewing, encoded byte length, and exact
equality. Unchecked formation MUST require an unsafe boundary; inspection and exact equality of
already valid strings SHALL remain safe. The intrinsics MUST NOT allocate, normalize, compare with
locale policy, traverse graphemes, recognize stdlib `String`, or decide owned storage behavior.
Each string intrinsic SHALL declare normalized availability for every current execution target,
and backend preparation SHALL reject reachable use for any target absent from that declaration
before target layout and MIR lowering.

#### Scenario: Audit the intrinsic catalog

- **WHEN** tooling enumerates the deterministic intrinsic catalog after this change
- **THEN** it finds the narrow string view operations with exact signatures, unsafe classification,
  normalized evaluator/native/Wasm availability, and no owning-String operation

#### Scenario: Use safe string inspection

- **WHEN** ordinary stdlib code asks an existing valid `string` for its bytes and byte length
- **THEN** it may call the inspection primitives outside unsafe because they cannot violate text or ownership invariants
