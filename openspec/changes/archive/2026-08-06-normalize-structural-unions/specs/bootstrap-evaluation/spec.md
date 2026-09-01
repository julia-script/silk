## ADDED Requirements

### Requirement: Evaluation carries immutable tagged union values

Evaluation SHALL represent a union as one immutable logical value containing its canonical union
type, active nominal member identity, and complete member payload. Injection SHALL install the
source member, widening SHALL remap that member into the target union without changing the payload,
and calls, returns, aggregate storage, moves, and writes SHALL preserve the same active identity.

#### Scenario: Evaluate injection and widening

- **WHEN** a `Token` is injected into `Token | End` and widened to `Token | End | Fault`
- **THEN** evaluation retains the complete `Token` payload under the canonical wider type

#### Scenario: Evaluate a union inside an array

- **WHEN** a fixed array stores values contextually injected into one union element type
- **THEN** each element retains its own active member and complete immutable payload

### Requirement: Evaluation cleans only the active union payload

Evaluation SHALL execute union cleanup from the canonical active member and ownership plan, releasing
that payload exactly once and performing no inactive-member cleanup. Trace events for injection,
widening, transport, replacement, and cleanup SHALL use canonical type/member identities and exact
source provenance without exposing numeric tags as source values.

#### Scenario: Trace replacement cleanup

- **WHEN** a mutable union containing `Token` is replaced by one containing `End`
- **THEN** the trace records one `Token` cleanup before one committed replacement and no `End` cleanup for the old value
