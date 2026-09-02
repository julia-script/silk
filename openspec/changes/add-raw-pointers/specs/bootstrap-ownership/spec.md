## ADDED Requirements

### Requirement: Raw pointers are Copy and loan-free

A raw pointer type SHALL be Copy through the sealed Copy property, SHALL contribute no cleanup
obligation to any aggregate, array, or union containing it, and SHALL be storable in any position
that admits a Copy value. Forming a pointer from a borrow SHALL be an ordinary read of that borrow
and SHALL create no loan on the root.

#### Scenario: Store a pointer in a struct

- **WHEN** a struct declares a field `handle: *mut Opaque` and no other non-Copy field, and declares `impl Copy`
- **THEN** conformance accepts the struct as Copy and its cleanup plan is empty

#### Scenario: Forming a pointer leaves the root movable

- **WHEN** code forms a pointer from `&mut value` and then moves `value`
- **THEN** ownership records no loan conflict
