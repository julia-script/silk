## MODIFIED Requirements

### Requirement: Native Unix-family providers preserve the portable contracts

The standard library SHALL provide separate stateless `OsSystemClock` and `OsMonotonicClock`
ordinary-source implementations selected for Darwin ARM64 with system libc and GNU Linux x86-64/ARM64 with GNU libc. Unsupported and no-libc profiles SHALL leave their native provider modules empty. The
system provider SHALL use ordinary unsafe C declarations for `clock_gettime` and `clock_getres`
with `CLOCK_REALTIME` and a C-layout `timespec`; it MUST NOT call an `Intrinsic.os*` operation or a
compiler-generated `silk_os_*` runtime function. The monotonic provider SHALL use ordinary unsafe C declarations, with source-owned read/resolution validation and platform wait policy, and SHALL use one platform
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

Clock services and pure source providers SHALL remain analyzable on every target. Native provider members SHALL be selected only for admitted Darwin/system-libc or GNU/Linux/GNU-libc profiles. Reachable operations SHALL use ordinary foreign imports, with no clock-specific compiler-runtime inventory. No target SHALL receive an ambient clock implementation or generated clock shim.

#### Scenario: Import an unused OS provider on LLVM-generated WebAssembly

- **WHEN** a WebAssembly program imports the OS module without requesting a native member
- **THEN** source selection leaves it empty and no clock import is emitted

#### Scenario: Reach the system provider on LLVM-generated WebAssembly

- **WHEN** WebAssembly or no-libc source imports OsSystemClock or OsMonotonicClock
- **THEN** source selection reports the missing member before emission

#### Scenario: Link only a selected clock operation

- **WHEN** native source reaches system now but no resolution or wait
- **THEN** its foreign inventory contains only clock_gettime for clock operations, with no clock runtime symbols or C fragment

#### Scenario: Expose POSIX clocks in a combined shim

- **WHEN** source reaches native clocks alongside filesystem or typed-failure termination
- **THEN** clock declarations and policy remain ordinary source and do not require a generated clock prelude

#### Scenario: Keep a clock-only shim independent of filesystem support

- **WHEN** a native program reaches only a monotonic clock operation
- **THEN** no clock shim is generated; ordinary source calls libc without filesystem support
