## ADDED Requirements

### Requirement: Inspect elaborated HIR with typed provenance

The syntax lab SHALL present the elaborated HIR of the current source: each function with its
canonical identity state and normalized contract, and its body as typed core operations in
evaluation order, with unavailable states explicit. Hovering or focusing an HIR expression SHALL
reveal its resolved type and exact source span. The lab's semantic views SHALL read elaboration
facts.

#### Scenario: View a function's HIR

- **WHEN** the inspected source elaborates a function returning a resolved call
- **THEN** the HIR view lists the function's contract and a typed call operation referencing the target's canonical identity

#### Scenario: Reveal type and span on hover

- **WHEN** a developer hovers an HIR expression entry
- **THEN** the entry reveals its resolved type and exact half-open source span

#### Scenario: Keep unavailable HIR explicit

- **WHEN** the inspected source contains an unknown call target
- **THEN** the HIR view marks the expression unavailable rather than fabricating a typed operation
