## MODIFIED Requirements

### Requirement: MIR verifies allocation and cleanup as a structured DAG

MIR SHALL contain compiler-planned operations and regions for checked layout formation, general
allocator witness dispatch, typed allocation outcomes, self-contained reclaim authority, raw typed
buffer construction, lexical Slot projection and value operations, shared bounds-checked reads of
recursively Copy elements including structural unions, initialization commit or rollback,
restricted Drop calls, explicit drop, and automatic field cleanup. A structural-union copy SHALL
retain its canonical sum type, active-member tag lane, complete payload lanes, and source/result
provenance. Verification SHALL reject layout/type/provenance mismatch, slot escape, conflicting live
loans, a shared read without shared buffer and canonical Copy-element provenance, a union copy with
any non-Copy member, use after consumption, invalid hook contracts, missing cleanup on a structured
exit, duplicate release, and allocator-kind or named-scope operations. Runtime initializedness
inside an unsafe buffer remains an unsafe program invariant rather than a verifier claim.

#### Scenario: Encode an exhausted construction attempt

- **WHEN** allocation fails before a construction guard receives storage
- **THEN** MIR carries the `OutOfMemory` branch with cleanup for earlier live owners and no allocation release operation for the rejected request

#### Scenario: Encode partial rollback

- **WHEN** a later typed failure exits after a guard initialized a prefix
- **THEN** the DAG orders the guard hook, initialized-element destruction, allocation release, and unchanged failure propagation without a control back-edge

#### Scenario: Reject forged reclaim authority

- **WHEN** malformed MIR attaches a release operation to a different allocation identity or inactive ticket
- **THEN** verification rejects the program before evaluation or backend emission

#### Scenario: Preserve a shared structural-union read

- **WHEN** HIR contains a valid shared raw-buffer read of a structural union whose members are all Copy
- **THEN** MIR records the buffer, index, canonical union element and result types, checked bounds, shared access, and source provenance without a Slot or storage-state transition

#### Scenario: Reject a structural-union read with a move-only member

- **WHEN** malformed MIR requests a Slot or shared raw-buffer copy for a union containing one non-Copy member
- **THEN** verification rejects the operation before evaluation or backend emission
