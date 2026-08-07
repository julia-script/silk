## ADDED Requirements

### Requirement: Allocations are affine owners with scope-bounded cleanup

Ownership analysis SHALL treat each allocation as one move-only owner tied to its destination scope
and original reclaim capability. Moves SHALL transfer the complete cleanup obligation, explicit
`drop` SHALL consume it, and every structured exit SHALL release remaining live allocations in
reverse acquisition order before outer values or failures leave that scope. The analysis MUST reject
double consumption, scope escape, provider-underlives-scope, and cleanup while a conflicting borrow
is live.

#### Scenario: Move an allocation owner

- **WHEN** one allocation binding is moved to another binding
- **THEN** only the destination remains live and exactly one cleanup obligation reaches later exits

#### Scenario: Clean an early return

- **WHEN** an early return exits a named scope with two live allocations
- **THEN** ownership plans release of the second allocation then the first before the return outcome

### Requirement: Slot places borrow the allocation and remain unsafe

Ownership analysis SHALL treat each selected `Slot<T>` as a lexical exclusive place borrowing its
allocation. The allocation MUST NOT move, drop, or release while that place is live, and the slot
MUST NOT escape into safe owned storage. The compiler SHALL check provenance and the ordinary affine
transfer of a `T` written to or moved from the place, but SHALL NOT claim to prove runtime-indexed
initializedness or disjointness; those are unsafe-code invariants.

#### Scenario: Move an initialized slot value

- **WHEN** unsafe code initializes a slot and later moves its `T` value out
- **THEN** the value cleanup obligation moves to the destination and raw allocation cleanup never treats the emptied bytes as a value

#### Scenario: Reject allocation movement under a slot loan

- **WHEN** code attempts to move or drop the allocation while a selected slot place is live
- **THEN** ownership rejects the owner operation with the slot borrow retained as provenance

### Requirement: Restricted drop hooks precede field cleanup

Ownership checking SHALL accept at most one restricted drop hook for an affine nominal struct only
when its body is infallible, non-allocating, requirement-free, cannot move from or replace `self` or
its fields, and cannot escape a borrow of `self`. Cleanup planning SHALL invoke the hook exactly once
before recursive declaration-ordered field cleanup on every owner-ending path.

#### Scenario: Clean a vector-shaped owner

- **WHEN** an affine struct's hook drops a runtime initialized prefix from its allocation field
- **THEN** the hook completes before the allocation field's raw bytes are released exactly once
