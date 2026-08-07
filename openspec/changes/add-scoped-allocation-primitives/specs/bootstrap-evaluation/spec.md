## ADDED Requirements

### Requirement: Logical evaluation models scoped allocation authoritatively

The evaluator SHALL use deterministic allocation identities and addressable storage with explicit
layout, scope, allocator origin, logical slot contents, and stable cleanup control blocks. A
deterministic provider SHALL support failure at a requested allocation ordinal. Evaluation SHALL
expose ordered acquisition, slot operations, drop-hook execution, disarm, and release events without
relying on JavaScript object identity or garbage collection.

#### Scenario: Inject every allocation failure

- **WHEN** the quota provider fails one requested allocation ordinal
- **THEN** evaluation returns typed `OutOfMemory`, releases every earlier live allocation in LIFO order, and retains no live allocation records from the failed request

#### Scenario: Reuse the evaluator after failure

- **WHEN** a successful evaluation follows an injected allocation failure in the same process
- **THEN** allocation ordinals, scope records, and slot contents reflect a fresh valid request with no poisoned state
