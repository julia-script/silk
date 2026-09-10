## Purpose

Define native secure-fill source selection, exact foreign contracts and initialized-buffer transfer states with deterministic conformance evidence.

## ADDED Requirements

### Requirement: Selected entropy declarations match the native ABI

OsRandom SHALL be selected only for Darwin ARM64/system libc and GNU Linux x86-64 or ARM64/GNU libc. Darwin SHALL call void arc4random_buf with a mutable byte pointer and size_t length. GNU SHALL call getrandom with a mutable byte pointer, size_t length and unsigned C int flags, returning ssize_t, and SHALL capture its current-thread C int errno only after failure. Construction SHALL make no native call. Unsupported profiles SHALL expose no native member or replacement host import.

#### Scenario: Select Darwin's void contract

- **WHEN** a nonempty fill executes on Darwin
- **THEN** source invokes arc4random_buf once and does not interpret a fabricated status or read errno

#### Scenario: Reject unavailable source members

- **WHEN** Wasm or no-libc source requests OsRandom
- **THEN** source selection rejects the missing member before emission without an entropy runtime adapter

### Requirement: GNU source completes secure fills through bounded progress

GNU source SHALL request at most 256 bytes with GRND_NONBLOCK=1, advance only a positive committed count no larger than the request, and retry EINTR without advancing. Empty fills SHALL make no call. Zero progress, invalid counts and other failures SHALL trap without returning partial success or weak output.

#### Scenario: Preserve retry offsets

- **WHEN** a positive short fill is followed by EINTR and then further progress
- **THEN** the next request resumes at the exact committed prefix with the original remaining extent

#### Scenario: Fail without waiting for readiness

- **WHEN** getrandom reports EAGAIN
- **THEN** the native provider traps without readiness polling, device fallback or a typed failure channel

### Requirement: Memory initialization and fresh entropy are distinct

The public input SHALL already be initialized exclusive byte storage. After a partial native write, the committed prefix MAY hold fresh entropy while the remaining bytes retain their initialized prior values. Only completion across the whole slice SHALL return success. Failure SHALL NOT imply rollback, unchanged output or fatal-trap cleanup guarantees. No uninitialized-buffer public API SHALL be added.

#### Scenario: Fail after partial modification

- **WHEN** a native request modifies a prefix and a subsequent request fails
- **THEN** the remaining bytes stay memory-valid and execution traps instead of returning a partial secure fill

### Requirement: Conformance verifies active imports and deterministic outcomes

Pinned independent headers and C fixtures SHALL verify declarations, scalar layout and constants, and actual native objects SHALL contain only the selected entropy imports. Debug and optimized source/C fixtures SHALL execute on every admitted native target; missing supplies SHALL fail, and unverified LTO SHALL be rejected. Real entropy tests SHALL assert successful completion without statistics, two-fill inequality or secret-byte logging. All migrated intrinsic, reserved runtime, lowering and generated C paths SHALL be deleted.

#### Scenario: Verify all native lanes

- **WHEN** the conformance runner executes the admitted matrix
- **THEN** Darwin ARM64 and GNU x86-64/ARM64 provide actual separately compiled C/source execution evidence in both modes rather than skipped or simulated lanes
