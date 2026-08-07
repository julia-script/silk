## ADDED Requirements

### Requirement: Ownership unifies Effect captures allocation and Drop

Ownership SHALL treat Effect environments, allocations, raw buffers, vectors, external-resource
wrappers, and failure payloads through the same affine model. It SHALL transfer cleanup on move,
end lexical provider borrows after calls, reject illegal repeat or slot escape, and schedule Drop
exactly once on every structured exit and typed failure. It MUST NOT schedule normal cleanup for a
trap.

#### Scenario: Move an allocated Vector through typed failure control

- **WHEN** a Vector is moved into a repeatable-ineligible Effect that may fail before consuming it
- **THEN** each reachable path has exactly one owner and cleanup plan, and a second run is rejected
