## ADDED Requirements

### Requirement: Referent places preserve borrowed ownership

A bare referent projection SHALL read only when the target has sealed `Copy` conformance and SHALL
leave the backing owner available. It SHALL NOT move an affine target through borrowed storage.
Shared referents SHALL permit only shared reborrows and reads, while exclusive referents SHALL also
permit exclusive reborrows and replacement with ordinary cleanup.

#### Scenario: Read a Copy scalar through a shared reference

- **WHEN** `value.*` reads `u32` from `value: &u32`
- **THEN** the result is copied and the backing owner remains available

#### Scenario: Reject an affine referent read

- **WHEN** a bare projection attempts to read a non-Copy target
- **THEN** ownership analysis rejects the borrowed move

#### Scenario: Reject mutation through shared access

- **WHEN** source assigns through a shared referent or requests `&mut value.*`
- **THEN** ownership analysis rejects the access strengthening

#### Scenario: Replace through exclusive access

- **WHEN** source assigns a compatible value through an exclusive referent
- **THEN** the previous referent is cleaned up exactly once
- **AND** the exclusive owner is restored after the access ends
