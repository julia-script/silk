## MODIFIED Requirements

### Requirement: Evaluation carries immutable tagged union values

Evaluation SHALL represent a union as one immutable logical value containing its canonical union
type, active ordinary member identity, and complete member payload. Injection SHALL install the
source member, widening SHALL remap that member into the target union without changing the payload,
and calls, returns, aggregate storage, moves, and writes SHALL preserve the same active identity.

#### Scenario: Evaluate injection and widening

- **WHEN** an `i32` is injected into `i32 | Token` and widened to `i32 | Token | Fault`
- **THEN** evaluation retains the complete scalar payload under the canonical wider type

#### Scenario: Evaluate represented executable members

- **WHEN** an exact callable or opaque Effect value is injected, stored, projected, and invoked or run
- **THEN** evaluation preserves its exact finite representation and produces the same result as the unwrapped value

#### Scenario: Evaluate a union inside an array

- **WHEN** a fixed array stores values contextually injected into one ordinary union element type
- **THEN** each element retains its own active member and complete immutable payload
