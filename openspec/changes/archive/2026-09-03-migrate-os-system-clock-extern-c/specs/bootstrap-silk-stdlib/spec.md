## MODIFIED Requirements

### Requirement: Canonical source exports separate native clock providers

The manifest SHALL export `silk/os_system_clock` and `silk/os_monotonic_clock` as separate native
provider actors. Each module SHALL define one stateless provider, an infallible constructor, the
ordinary source operations needed for its matching service conformance, and documented fatal and
target limitations. Portable service signatures MUST NOT mention either provider, a platform clock
identifier, runtime symbol, target selector, or native status protocol.

`silk/os_system_clock` SHALL own its libc boundary as ordinary source declarations over a C-layout
record. Its reachable system-clock operations SHALL contribute only the corresponding foreign C
symbols, never a sealed system-clock intrinsic or compiler-owned OS runtime symbol.

#### Scenario: Construct providers without reading time

- **WHEN** an application constructs either OS clock provider and does not invoke a clock operation
- **THEN** construction completes without consulting the host and contributes no reachable clock
  runtime or foreign symbol

#### Scenario: Keep provider modules independent

- **WHEN** an application imports and provides only `OsSystemClock`
- **THEN** its source closure does not require `OsMonotonicClock` and its executable closure gains
  no monotonic wait support
