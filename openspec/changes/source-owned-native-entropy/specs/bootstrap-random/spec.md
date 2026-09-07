## MODIFIED Requirements

### Requirement: Random owns one exact-fill provider primitive

The `Random` service SHALL own exactly one provider operation that fills a complete exclusive byte
slice of already initialized bytes and returns no recoverable failure. Every provider and the public wrapper MUST complete an
empty slice without consulting or advancing its source. A nonempty successful call MUST replace
every requested byte with fresh provider data; short successes are not observable at this public boundary. Provider failure
or malformed output SHALL terminate as a fatal defect rather than permit application fallback to
weaker data.

#### Scenario: Fill a complete slice

- **WHEN** a caller passes a nonempty exclusive byte slice to `Random.fillBytes`
- **THEN** the operation returns only after every element contains fresh provider data

#### Scenario: Fill an empty slice through either call surface

- **WHEN** an empty slice is passed through the public wrapper or direct `Random.fillBytes` service dispatch
- **THEN** the call succeeds without consulting or advancing the active provider

#### Scenario: Refuse weak recovery

- **WHEN** an official provider cannot complete a secure request
- **THEN** execution terminates without returning partial data or a recoverable value that invites an insecure fallback

### Requirement: Official OS random support is native, non-waiting, and pay-for-use

The official OsRandom provider SHALL use ordinary selected libc declarations for Darwin ARM64/system libc and GNU Linux x86-64 or ARM64/GNU libc. GNU SHALL request GRND_NONBLOCK and trap on unavailable entropy rather than wait for readiness. Darwin SHALL use the secure void arc4random_buf contract without an invented readiness flag or failure status; it SHALL make no stronger latency promise than that native call. Unsupported and no-libc profiles SHALL expose no native provider member. No generated OS-random adapter, weak fallback, raw syscall, Windows/WASI/browser host API or ambient default SHALL be installed.

#### Scenario: Link one native random consumer

- **WHEN** a native executable reaches `OsRandom.fillBytes`
- **THEN** its artifact includes only selected ordinary entropy foreign imports and returns complete CSPRNG output under the platform contract

#### Scenario: Fail rather than wait for entropy

- **WHEN** GNU getrandom reports that its CSPRNG is not ready
- **THEN** `OsRandom` terminates without waiting or substituting predictable bytes

#### Scenario: Keep portable code free of OS random

- **WHEN** an executable reaches only `InsecureRandom` or a fixed `InsecureSeed` provider
- **THEN** it includes no OS-random runtime symbol

#### Scenario: Reject reachable LLVM-generated WebAssembly OS random

- **WHEN** an LLVM-generated WebAssembly entry reaches the official `OsRandom` provider
- **THEN** source selection reports the unavailable native member before emission
