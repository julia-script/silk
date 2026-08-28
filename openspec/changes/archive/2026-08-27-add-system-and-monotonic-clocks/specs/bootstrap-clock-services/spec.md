## Purpose

Define portable, provider-replaceable system and monotonic clock contracts together with explicit
native Unix-family providers for reading time, querying resolution, and waiting on monotonic time.

## ADDED Requirements

### Requirement: Instant has one canonical signed-seconds representation

The standard library SHALL define `Instant` in `silk/system_clock` with private `seconds: i64` and
`nanoseconds: i64` fields, a public validating constructor, and public field accessors. Every
constructed or service-returned `Instant` SHALL satisfy
`0 <= nanoseconds < 1_000_000_000`; its value SHALL be `seconds * 1_000_000_000 + nanoseconds`
nanoseconds on that clock's timeline. Negative instants SHALL use the floored split, so one
nanosecond before the Unix epoch is `{ seconds: -1, nanoseconds: 999_999_999 }`.

#### Scenario: Represent a pre-epoch system instant

- **WHEN** a system-clock provider reports one nanosecond before the Unix epoch
- **THEN** it returns `Instant { seconds: -1, nanoseconds: 999_999_999 }`

#### Scenario: Return a canonical fraction

- **WHEN** either clock returns an `Instant`
- **THEN** its `nanoseconds` field is non-negative and less than one billion

#### Scenario: Reject a noncanonical construction

- **WHEN** ordinary source tries to construct an `Instant` with negative nanoseconds or at least
  one billion nanoseconds
- **THEN** the public constructor traps rather than creating a second representation of a mark

#### Scenario: Implement a clock outside the defining module

- **WHEN** an ordinary user-defined provider needs to return an `Instant`
- **THEN** it can construct a canonical value and observe both components through public actor
  functions without receiving field-mutation access

### Requirement: SystemClock reports Unix-epoch civil time

`silk/system_clock` SHALL define an exclusive `SystemClock` service whose `now` operation returns an
`Instant` measured from `1970-01-01T00:00:00Z` and whose `getResolution` operation returns the
positive nominal resolution reported by the provider in whole nanoseconds. That value describes the
provider or platform tick and MAY be a lower bound on actual observable precision; callers MUST NOT
interpret it as an empirical guarantee that every adjacent reading differs by that amount. The clock
MAY be adjusted forwards or backwards between calls and MUST NOT be presented as an elapsed-time source.
Both operations SHALL be infallible in their typed Effect contract; an implementation that cannot
produce a representable canonical result or a positive `u64` resolution SHALL terminate through the
existing fatal-trap boundary.

#### Scenario: Read Unix-epoch time

- **WHEN** a provider's external reference is exactly the Unix epoch
- **THEN** `SystemClock.now` returns `Instant { seconds: 0, nanoseconds: 0 }`

#### Scenario: Observe a clock reset

- **WHEN** the external reference is adjusted backwards between two system-clock reads
- **THEN** the second `Instant` may precede the first without violating the service contract

#### Scenario: Report nominal system-clock resolution

- **WHEN** the platform reports a positive resolution smaller than the precision observed by an
  application
- **THEN** `getResolution` returns the platform-reported whole-nanosecond lower bound without trying
  to measure empirical precision

#### Scenario: Reject an unrepresentable host value fatally

- **WHEN** the selected provider cannot represent its reading or resolution in the public types
- **THEN** execution traps rather than returning a malformed `Instant`, zero-filling a value, or
  introducing an undeclared typed failure

### Requirement: MonotonicClock reports non-decreasing provider-local marks

`silk/monotonic_clock` SHALL define an exclusive `MonotonicClock` service whose `now` operation returns
an `Instant` on an unspecified provider-local timeline and whose `getResolution` returns its positive
nominal tick duration in whole nanoseconds, which MAY be a lower bound on actual observable precision.
Successive reads from one unchanged provider SHALL be non-decreasing,
but equal successive values SHALL be permitted. A mark's comparison and absolute-wait meaning SHALL
be defined only for the same logical provider timeline that produced it; the shared `Instant` shape
does not grant system-clock instants or marks from another provider that meaning.
`getResolution` SHALL return a positive `u64`. An implementation that cannot produce a representable
canonical mark or positive resolution SHALL trap rather than return a malformed value or introduce
an undeclared typed failure.

#### Scenario: Read equal monotonic values

- **WHEN** two reads occur within one provider tick
- **THEN** the provider may return equal canonical `Instant` values

#### Scenario: Read advancing monotonic values

- **WHEN** the provider advances between two reads without being replaced
- **THEN** the second value does not precede the first

#### Scenario: Keep mark provenance explicit

- **WHEN** a caller has a system-clock instant or a mark from another monotonic provider
- **THEN** the API documentation identifies it as invalid input to comparisons or `waitUntil` on
  the active monotonic provider even though the common struct representation is assignable

#### Scenario: Reject an unusable monotonic reading fatally

- **WHEN** a monotonic provider cannot represent its reading canonically or its positive resolution
  as `u64`
- **THEN** execution traps rather than returning a malformed mark, a zero resolution, or an
  undeclared typed failure

### Requirement: Monotonic waits are deadline-based and native waits block

`MonotonicClock.waitUntil(when)` SHALL return only once the active provider's logical monotonic value
is at least `when`, and SHALL return immediately when that value has already been reached.
`MonotonicClock.waitFor(howLong)` SHALL account for at least `howLong` nanoseconds on the same
provider timeline and SHALL be equivalent to taking one start mark, adding the duration without
drift, and waiting for that absolute deadline. A virtual provider MAY satisfy either operation by
advancing its own timeline without wall-clock suspension. A zero duration SHALL complete without a
positive timeline advance. If the duration cannot be added to the start mark as a canonical
`Instant`, `waitFor` SHALL trap. Signal interruption or spurious early return from a native wait MUST
NOT cause either operation to complete before its monotonic condition is satisfied.

The official OS provider's first native waits SHALL be blocking host operations. They SHALL NOT park
only the current Silk `Execution`, install a scheduler timer, promise Fiber interruption, or permit
other tasks on a single-threaded scheduler to advance while the host thread is asleep.

#### Scenario: Return for a past deadline

- **WHEN** `waitUntil` receives a valid same-provider mark less than or equal to the current mark
- **THEN** it returns without suspending the calling thread

#### Scenario: Wait across interruption

- **WHEN** a native wait is interrupted before the requested monotonic deadline
- **THEN** the provider resumes waiting against the original absolute deadline

#### Scenario: Avoid relative-wait drift

- **WHEN** `waitFor` is interrupted one or more times
- **THEN** time spent handling interruptions counts toward the original requested duration rather
  than being added once per retry

#### Scenario: Complete a zero wait

- **WHEN** `waitFor(0)` is called
- **THEN** it completes without requiring the monotonic clock to advance

#### Scenario: Advance virtual time without sleeping

- **WHEN** a deterministic provider satisfies a future deadline by advancing its own scripted
  timeline
- **THEN** the operation may return without suspending the host thread after its logical mark has
  reached the deadline

#### Scenario: Trap an unrepresentable relative deadline

- **WHEN** adding `howLong` to the start mark exceeds the canonical `Instant` seconds range
- **THEN** `waitFor` traps instead of wrapping or waiting for a different deadline

#### Scenario: Block the host thread

- **WHEN** an official OS monotonic wait runs inside a task owned by `LocalScheduler`
- **THEN** no promise is made that another task runs before the wait returns

### Requirement: Clock services are explicit replaceable capabilities

The two clock services SHALL use exclusive service requirements, SHALL expose ordinary module-level
wrappers for every service operation, and SHALL accept ordinary user-defined providers with no
compiler registration. Importing either service or an OS provider SHALL NOT create an ambient clock
or satisfy an entry requirement; application source SHALL provide each selected implementation
explicitly and MAY replace it lexically for deterministic tests. Exclusive access SHALL allow a
scripted provider to advance a sequence or record waits without changing the service's observable
clock guarantees.

#### Scenario: Supply a fixed system clock

- **WHEN** an application provides an ordinary source value implementing `SystemClock`
- **THEN** `now` and `getResolution` dispatch to that value without an OS host or compiler-known
  provider identity

#### Scenario: Advance a scripted provider

- **WHEN** a deterministic monotonic provider updates its private timeline during `now` or a wait
- **THEN** the service's `&mut MonotonicClock` requirement provides exclusive access to that state

#### Scenario: Supply independent clock providers

- **WHEN** one Effect requires both `SystemClock` and `MonotonicClock`
- **THEN** source can provide the two capabilities independently and neither provider satisfies the
  other's requirement

#### Scenario: Leave an entry unresolved

- **WHEN** an effectful entry reaches a clock operation without explicit provision
- **THEN** ordinary requirement closure rejects the entry rather than installing an OS default

### Requirement: Native Unix-family providers preserve the portable contracts

The standard library SHALL provide separate stateless `OsSystemClock` and `OsMonotonicClock`
ordinary-source implementations for every native target currently supported by the compiler. The
system provider SHALL use the platform's real-time Unix-epoch clock. The monotonic provider SHALL
use one platform monotonic clock consistently for reads, resolution, and waits, SHALL tolerate
interruptions, and SHALL never substitute the adjustable system clock for elapsed-time behavior.

Linux and macOS SHALL both support all six public clock operations. A supported Unix-family target
without a native absolute monotonic sleep operation SHALL reach the same semantics by repeatedly
reading the selected monotonic clock and sleeping only the positive remainder. Platform differences
in the unspecified monotonic origin and whether suspend time advances SHALL remain permitted.
The Linux implementation without `librt` SHALL target `glibc >= 2.17`; support for an older glibc
clock ABI requires a separate target-baseline change.

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

### Requirement: Native clock support is reachable-only and excludes direct Wasm

Clock service declarations and pure source providers SHALL remain analyzable on every target. The
OS clock providers' primitive calls SHALL be native-only, validated only after executable
reachability, and linked only when reachable. Direct WebAssembly SHALL receive no invented clock
import or implementation in this change and SHALL reject a reachable OS clock operation through the
existing target-availability diagnostic.

#### Scenario: Import an unused OS provider on direct Wasm

- **WHEN** a direct-Wasm program imports an OS clock module but reaches no native clock primitive
- **THEN** compilation succeeds without a clock import or runtime symbol

#### Scenario: Reach an OS provider on direct Wasm

- **WHEN** the same program reaches an OS clock provider operation
- **THEN** target validation reports the native-only intrinsic as unavailable before emission

#### Scenario: Link only a selected clock operation

- **WHEN** a native program reaches system `now` but no resolution or monotonic operation
- **THEN** its runtime shim contains only the native clock support required by that reachable
  primitive

#### Scenario: Keep a clock-only shim independent of filesystem support

- **WHEN** a native program reaches a clock primitive and no filesystem or child-process primitive
- **THEN** the selected C source contains the minimal status and clock prelude plus the reachable
  clock symbol, without unrelated filesystem macros, helpers, or platform assumptions

#### Scenario: Expose POSIX clocks in a combined shim

- **WHEN** a clock symbol is combined with standard streams, typed-failure termination, or another
  runtime fragment that includes system headers
- **THEN** the translation unit defines its platform and POSIX feature-test macros before every
  system header and the strict-C11 compiler sees all required clock declarations
