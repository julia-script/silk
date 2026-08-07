## ADDED Requirements

### Requirement: Evaluation is the Effect and allocation oracle

The evaluator SHALL model lazy construction, capture persistence, one-shot rejection, retry attempts,
provider acquisition, allocation identity, initialized slots, Vector state, explicit drop, Drop order,
and deterministic allocation failure without relying on JavaScript garbage collection or object
identity.

#### Scenario: Sweep every allocation failure

- **WHEN** a deterministic allocator fails each allocation ordinal of a Vector-building Effect in turn
- **THEN** evaluation preserves the original vector or drops every committed owner exactly once and can run a fresh program afterward
