## Purpose

Define source-owned native clock declarations, canonical result validation and interruption-safe deadline waits with reproducible target ABI evidence.

## ADDED Requirements

### Requirement: Native clock availability and declarations match selected supplies

Native providers SHALL exist only for Darwin ARM64 with system libc and GNU Linux x86-64/ARM64 with GNU libc. Their foreign declarations SHALL match selected clock scalar signedness, Timespec size/alignment/offsets, constants and signatures. Construction SHALL make no foreign call. Portable clock services SHALL remain replaceable on unsupported profiles without native imports.

#### Scenario: Select Darwin clock identifiers

- **WHEN** Darwin ARM64 with system libc is selected
- **THEN** clock identifiers use unsigned 32-bit values, realtime 0 and monotonic 6, with signed 64-bit Timespec fields at offsets 0 and 8

#### Scenario: Select GNU clock identifiers

- **WHEN** GNU Linux x86-64 or ARM64 with GNU libc is selected
- **THEN** clock identifiers use signed 32-bit values, realtime 0 and monotonic 1, and absolute wait flag 1

#### Scenario: Select an unsupported native provider

- **WHEN** a Wasm or no-libc profile imports a native clock provider
- **THEN** source selection reports the provider unavailable without a late native adapter failure

### Requirement: Source validates native clock results without a typed failure channel

Native reads SHALL reject syscall failure or noncanonical fractions. Native monotonic seconds SHALL be nonnegative. Resolution SHALL be positive and representable as u64 whole nanoseconds, rejecting negative components, noncanonical fractions and overflow. Rejection SHALL use the existing fatal trap and SHALL NOT add a typed failure channel. Civil time SHALL permit canonical negative seconds.

#### Scenario: Reject malformed results

- **WHEN** a host returns a negative fraction, a fraction at least one billion, zero resolution or an overflowing resolution
- **THEN** the provider traps before returning a malformed value

### Requirement: Wait retries preserve the original deadline and native error convention

Relative waits SHALL take one initial monotonic read and use checked deadline arithmetic. GNU absolute waits SHALL retry only direct EINTR status using the same deadline and SHALL NOT read errno. Darwin relative waits SHALL recompute remaining time from the same deadline after success or interruption; failed nanosleep SHALL capture errno before another call. Past/zero waits SHALL require no positive clock advance. Failure, invalid deadlines or overflow SHALL trap. Native waits SHALL block the host thread and SHALL NOT promise task-local parking.

#### Scenario: GNU status is independent of errno

- **WHEN** clock_nanosleep returns EINTR while errno contains another value
- **THEN** it retries the identical absolute deadline, and any other nonzero direct status traps

#### Scenario: Darwin recomputes after interruption and early success

- **WHEN** nanosleep returns EINTR or success before the monotonic deadline
- **THEN** the next remaining interval is recomputed from a fresh reading against the original deadline with canonical fractional borrow

### Requirement: Clock conformance proves the actual selected boundary

Pinned independent C/header fixtures SHALL verify selected scalar and record layouts, constants and signatures. Deterministic source/C execution SHALL prove retry/error policy in debug and optimized modes on all admitted native targets. Required lanes SHALL fail when supplies or execution are missing. LTO SHALL be rejected until verified. Real-clock correctness SHALL use semantic invariants rather than elapsed-time thresholds. No migrated clock intrinsic, reserved runtime symbol or generated clock fragment SHALL remain.

#### Scenario: Missing required conformance supply

- **WHEN** a required native lane lacks its pinned headers, toolchain or runner
- **THEN** conformance fails instead of recording a skip as success
