## ADDED Requirements

### Requirement: Evaluation is the deterministic allocation oracle

The evaluator SHALL execute compiler-planned allocation, logical addresses, reclaim tickets,
RawBuffer storage, Slot operations, initialization events, restricted hooks, explicit drop, and
automatic cleanup without relying on JavaScript object identity or garbage collection. It SHALL
support deterministic failure at each requested allocation ordinal, create no owner for a rejected
request, preserve self-contained owners after provider access ends, and expose bounded deterministic
events for acquisition, initialization, destruction, and release.

#### Scenario: Sweep allocation exhaustion

- **WHEN** the same construction program fails each allocation ordinal in turn
- **THEN** every run returns `OutOfMemory`, releases each successfully acquired owner exactly once, and permits a subsequent successful run in the same evaluator

#### Scenario: Drop after provision ends

- **WHEN** evaluation ends the exclusive allocator provider access before dropping the returned Allocation
- **THEN** release succeeds through the allocation's active ticket without looking up the current provider

#### Scenario: Observe hook-before-release order

- **WHEN** a guard owns initialized move-only elements and its backing allocation
- **THEN** the trace records element destruction by the hook before recursive field cleanup releases the bytes
