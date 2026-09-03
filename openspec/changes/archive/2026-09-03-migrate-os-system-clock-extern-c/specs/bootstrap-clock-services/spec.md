## MODIFIED Requirements

### Requirement: Native Unix-family providers preserve the portable contracts

The standard library SHALL provide separate stateless `OsSystemClock` and `OsMonotonicClock`
ordinary-source implementations for every native target currently supported by the compiler. The
system provider SHALL use ordinary unsafe C declarations for `clock_gettime` and `clock_getres`
with `CLOCK_REALTIME` and a C-layout `timespec`; it MUST NOT call an `Intrinsic.os*` operation or a
compiler-generated `silk_os_*` runtime function. The monotonic provider SHALL use one platform
monotonic clock consistently for reads, resolution, and waits, SHALL tolerate interruptions, and
SHALL never substitute the adjustable system clock for elapsed-time behavior.

Linux and macOS SHALL both support all six public clock operations. A supported Unix-family target
without a native absolute monotonic sleep operation SHALL reach the same semantics by repeatedly
reading the selected monotonic clock and sleeping only the positive remainder. Platform differences
in the unspecified monotonic origin and whether suspend time advances SHALL remain permitted. The
Linux implementation without `librt` SHALL target `glibc >= 2.17`; support for an older glibc clock
ABI requires a separate target-baseline change.

#### Scenario: Read both clocks on Linux

- **WHEN** a Linux native executable explicitly provides both OS clock providers
- **THEN** system time, monotonic time, both resolutions, and monotonic waits execute without an
  additional runtime library

#### Scenario: Wait on macOS without an absolute sleep API

- **WHEN** a macOS native executable waits for a future monotonic mark
- **THEN** it rechecks the same monotonic timeline until the deadline is reached and does not use
  the system clock as a substitute

#### Scenario: Preserve platform suspend behavior

- **WHEN** two supported platforms define different suspend-time behavior for their monotonic clocks
- **THEN** both implementations conform as long as each remains non-decreasing and its waits use
  the same timeline as its reads

### Requirement: Clock target behavior follows each provider boundary

Clock service declarations and pure source providers SHALL remain analyzable on every target. The
monotonic provider's primitive calls SHALL be native-only, validated only after executable
reachability, and linked only when reachable. A reachable system provider SHALL retain ordinary
foreign calls: native artifacts link them from libc, evaluator execution requires exact foreign-host
bindings, and direct WebAssembly emits imports from `silk:runtime/foreign@v1`. No target receives an
ambient or compiler-invented system-clock implementation.

#### Scenario: Import an unused OS provider on direct Wasm

- **WHEN** a direct-Wasm program imports an OS clock module but reaches no clock operation
- **THEN** compilation succeeds without a clock import or runtime symbol

#### Scenario: Reach the system provider on direct Wasm

- **WHEN** a direct-Wasm program reaches `OsSystemClock.now`
- **THEN** emission records a `clock_gettime` import in `silk:runtime/foreign@v1`, and instantiation
  requires the embedding host to provide that import

#### Scenario: Link only a selected clock operation

- **WHEN** a native program reaches system `now` but no resolution or monotonic operation
- **THEN** its foreign inventory contains only `clock_gettime` and its compiler-runtime inventory
  contains no system-clock symbol

#### Scenario: Keep a clock-only shim independent of filesystem support

- **WHEN** a native program reaches a monotonic clock primitive and no filesystem or child-process primitive
- **THEN** the selected C source contains the minimal clock prelude plus the reachable monotonic
  clock symbol, without unrelated filesystem macros, helpers, or platform assumptions

#### Scenario: Expose POSIX clocks in a combined shim

- **WHEN** a monotonic clock symbol is combined with standard streams, typed-failure termination,
  or another runtime fragment that includes system headers
- **THEN** the translation unit defines its platform and POSIX feature-test macros before every
  system header and the strict-C11 compiler sees all required clock declarations
