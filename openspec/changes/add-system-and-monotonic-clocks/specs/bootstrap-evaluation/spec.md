## ADDED Requirements

### Requirement: Evaluation injects independent native clock hosts

Bootstrap evaluation SHALL accept optional system-clock and monotonic-clock host providers as
separate configuration from each other and from filesystem, input, process, and stream hosts. The
system host SHALL supply canonical Unix-epoch reads and resolution. The monotonic host SHALL supply
canonical non-decreasing reads, resolution, and observable absolute waits. Evaluation SHALL
preserve exact `i64` and `u64` values without consulting JavaScript wall time unless the caller
explicitly chooses a real-time host implementation. Host validation SHALL require seconds in
`[-2^63, 2^63 - 1]`, fractions in `[0, 999_999_999]`, and resolutions in `[1, 2^64 - 1]`; it MUST
NOT truncate or wrap a `bigint` into those ranges.

#### Scenario: Evaluate with scripted clocks

- **WHEN** evaluation receives a fixed system host and a scripted monotonic host
- **THEN** clock operations return the exact injected values and waits advance or record only the
  scripted monotonic timeline

#### Scenario: Keep clock hosts independent

- **WHEN** evaluation receives only a system-clock host
- **THEN** a system read can complete while a reachable OS monotonic operation remains blocked for
  its own missing host

#### Scenario: Preserve wide clock values

- **WHEN** an injected host returns a valid clock component or resolution above JavaScript's exact
  integer range
- **THEN** evaluation retains the exact integer value without Number rounding

#### Scenario: Reject values just outside scalar ranges

- **WHEN** a host or scripted constructor supplies seconds outside `i64`, a fraction outside the
  canonical range, or a resolution outside positive `u64`
- **THEN** it returns explicit failure and commits no wrapped or partially initialized scalar output

#### Scenario: Reject an invalid scripted timeline

- **WHEN** a scripted-host constructor receives a malformed fraction, non-positive resolution, or
  decreasing monotonic sequence
- **THEN** it returns explicit host-construction failure data rather than accepting the script or
  throwing a JavaScript exception

#### Scenario: Record a past virtual wait

- **WHEN** a scripted monotonic host receives a deadline at or before its current mark
- **THEN** it records the completed wait without moving its timeline backwards

### Requirement: Missing evaluator clock hosts are explicit blocked data

A reachable native system-clock operation with no system host SHALL produce
`MissingSystemClock`; a reachable native monotonic operation with no monotonic host SHALL produce
`MissingMonotonicClock`. The blocked outcome and inspector presentation SHALL name the missing
capability and retain the trace preceding it. Evaluation MUST NOT fabricate epoch zero, reuse one
host for the other clock, read ambient process time, or throw a JavaScript exception for absence.

#### Scenario: Block a missing system clock

- **WHEN** evaluation reaches `OsSystemClock.now` with no injected system-clock host
- **THEN** it returns `Blocked(MissingSystemClock)` with the preceding deterministic trace

#### Scenario: Block a missing monotonic clock

- **WHEN** evaluation reaches an `OsMonotonicClock` read or wait with no injected monotonic host
- **THEN** it returns `Blocked(MissingMonotonicClock)` without consulting the system-clock host
