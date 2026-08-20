## MODIFIED Requirements

### Requirement: Generic bodies are checked once

The compiler SHALL elaborate and check each generic body once over its canonical type parameters.
Concrete specialization MUST substitute the verified generic facts and MUST NOT enable undeclared
operations through concrete duck typing or type-directed source branching. A type parameter SHALL
carry compiler-owned Copy evidence only when its declaration has an explicit `Copy` bound, and that
symbolic evidence SHALL propagate through nested generic calls.

#### Scenario: Propagate Copy evidence through a generic call

- **WHEN** `outer<T: Copy>` calls `inner<T>` whose parameter is also bounded by `Copy`
- **THEN** constraint solving forwards the caller's symbolic evidence and accepts the call without concrete specialization

#### Scenario: Reject an unbounded structural guess

- **WHEN** an unconstrained type parameter is used where `Copy` is required
- **THEN** generic checking rejects the use even if one later specialization would contain only Copy fields

#### Scenario: Preserve a generic whole-value move

- **WHEN** `identity<T>(value: T)` returns `move value`
- **THEN** ownership checks that transfer once over `T` and every concrete specialization reuses the proof

#### Scenario: Reject undeclared concrete behavior

- **WHEN** an unconstrained generic body calls an operation unavailable for its type parameter
- **THEN** the declaration is rejected before any concrete specialization can make the call appear valid
