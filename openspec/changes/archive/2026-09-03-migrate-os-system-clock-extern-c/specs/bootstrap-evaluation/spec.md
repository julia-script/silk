## MODIFIED Requirements

### Requirement: Evaluation injects a native monotonic-clock host

Bootstrap evaluation SHALL accept an optional monotonic-clock host provider as separate
configuration from filesystem, input, process, stream, and foreign-function hosts. The monotonic
host SHALL supply canonical non-decreasing reads, resolution, and observable absolute waits.
Evaluation SHALL preserve exact `i64` and `u64` values without consulting JavaScript wall time
unless the caller explicitly chooses a real-time host implementation. Host validation SHALL
require seconds in `[-2^63, 2^63 - 1]`, fractions in `[0, 999_999_999]`, and resolutions in
`[1, 2^64 - 1]`; it MUST NOT truncate or wrap a `bigint` into those ranges. The system clock MUST
NOT be exposed as a bespoke clock-host option; it uses the ordinary foreign-host table.

#### Scenario: Evaluate with a scripted monotonic clock

- **WHEN** evaluation receives a scripted monotonic host
- **THEN** reads return the exact injected values and waits advance or record only the scripted
  monotonic timeline

#### Scenario: Preserve wide monotonic-clock values

- **WHEN** an injected monotonic host returns a valid component or resolution above JavaScript's exact integer range
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

### Requirement: Missing evaluator clock boundaries are explicit blocked data

A reachable `OsSystemClock` operation SHALL use the ordinary foreign-host table and, without an
exact binding for `clock_gettime` or `clock_getres`, produce `ForeignHostUnavailable` naming the
symbol and C-class signature. A reachable native monotonic operation with no monotonic host SHALL
produce `MissingMonotonicClock`. The blocked outcome and inspector presentation SHALL name the
missing capability and retain the trace preceding it. Evaluation MUST NOT fabricate epoch zero,
reuse the monotonic host as a system clock, read ambient process time, or throw a JavaScript
exception for absence.

#### Scenario: Block a missing system clock

- **WHEN** evaluation reaches `OsSystemClock.now` without a `clock_gettime` foreign binding
- **THEN** it returns `Blocked(ForeignHostUnavailable(clock_gettime))` with the expected
  `(i32,*mut)->i32` signature and no ambient clock read

#### Scenario: Block a missing monotonic clock

- **WHEN** evaluation reaches an `OsMonotonicClock` read or wait with no injected monotonic host
- **THEN** it returns `Blocked(MissingMonotonicClock)` without consulting foreign system-clock
  bindings
