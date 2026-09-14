## Purpose

Define source-owned native TCP and pathname-Unix connections that publish one affine ByteDuplex
owner plus scoped convenience with cooperative readiness, bounded retry policy, and exact
descriptor cleanup.

## ADDED Requirements

### Requirement: Native sockets are selected ordinary-source providers

Canonical `silk.native_socket` SHALL be ordinary selected Silk source backed only by explicit
`extern "C"` imports. It SHALL be available exactly for aarch64 Apple Darwin with system libc and
aarch64 or x86-64 GNU/Linux with GNU ABI and libc. Its declarations and imports SHALL be absent on
LLVM-to-Wasm, no-libc, musl, Windows, and every other profile. The compiler SHALL NOT recognize a
socket, connection, HTTP actor, provider spelling, or platform socket policy.

#### Scenario: Select one admitted native profile

- **WHEN** the actor is realized for an admitted Darwin or GNU profile
- **THEN** its target-specific socket declarations and constants are available through ordinary
  source selection

#### Scenario: Keep portable targets free of socket imports

- **WHEN** a program is realized for LLVM-to-Wasm or another unsupported profile
- **THEN** the native actor contributes no socket member, foreign import, or implicit runtime shim

### Requirement: Connection acquisition publishes one affine owner

The actor SHALL expose `connectResolvedOwned` for a nonempty bounded ordered slice of owned numeric
`Endpoint` values and `connectUnixOwned` for a nonempty NUL-free absolute pathname whose encoded
bytes fit the selected platform's `sun_path` including its terminator. Each operation SHALL acquire
and return one affine `Connection` only after it reaches `Open`; callers cannot copy, clone, alias,
or extract its raw descriptor. The actor SHALL also expose `connectResolved` and `connectUnix` as
higher-ranked scoped conveniences that acquire through the corresponding owned operation,
exclusively lend the owner as `ByteDuplex`, and release it after every structured callback exit.
The connection SHALL have the observable phases `Connecting`, `Open`, `WriteClosed`, and `Closed`.
Fatal traps SHALL remain outside the structured cleanup guarantee.

#### Scenario: Publish only an established connection

- **WHEN** acquisition has not yet completed or has failed
- **THEN** no owner is returned, the scoped callback is not invoked, and every provisional
  descriptor is terminally released

#### Scenario: Reject an escaping connection

- **WHEN** a caller attempts to copy the owned connection, obtain its raw descriptor, or make a
  scoped callback retain its provider loan or request an ambient ByteDuplex alias
- **THEN** ownership, lifetime, or requirement-row analysis rejects the program before execution

#### Scenario: Close after every structured exit

- **WHEN** the callback succeeds, raises its own typed error, or is canceled while parked
- **THEN** release closes the descriptor exactly once and the callback outcome or cancellation
  remains observable

### Requirement: Connect options bound policy without renewing time

`ConnectOptions` SHALL admit `pollInterval` only in `1..1_000_000_000` nanoseconds and
`maxAttempts` only in `1..1024`, with defaults of 1,000,000 nanoseconds and 64 attempts. A resolved
endpoint slice SHALL be nonempty and no longer than `maxAttempts`. `connectResolvedOwned` and its
scoped `connectResolved` wrapper SHALL try candidates in input order, using a fresh descriptor for
every native attempt, and SHALL use one
optional absolute `MonotonicClock` deadline for the complete operation without resetting it between
candidates. Every native `connect` call, including a fresh-descriptor retry after `EINTR` or a full
GNU Unix backlog, SHALL count toward `maxAttempts`. `None` SHALL permit an established pending
operation to wait indefinitely. DNS work SHALL remain outside this operation and its deadline.

Permission, allocation/system-resource, timeout, and structured-cancellation failures SHALL stop
without another candidate. Refused or unreachable candidates MAY advance to the next candidate.
Exhausting the admitted attempt count SHALL report `AttemptsExhausted` with the exact count and last
per-attempt failure.

#### Scenario: Preserve candidate order and one deadline

- **WHEN** earlier endpoints refuse a connection and a later endpoint succeeds before the overall
  deadline
- **THEN** attempts use the supplied order and fresh descriptors while every wait compares the same
  absolute deadline

#### Scenario: Exhaust the finite attempt budget

- **WHEN** transient failures and fresh-descriptor retries consume `maxAttempts`
- **THEN** acquisition reports `AttemptsExhausted` with the final failure and exact attempt count
  without issuing another native connect

#### Scenario: Reject invalid options before acquisition

- **WHEN** an option is outside its admitted range, the endpoint slice is empty, or the slice exceeds
  `maxAttempts`
- **THEN** acquisition returns typed invalid input before creating a descriptor or sampling readiness

### Requirement: TCP setup fixes descriptor and signal policy before publication

Every TCP attempt SHALL create a nonblocking close-on-exec stream socket. GNU SHALL request
`SOCK_NONBLOCK | SOCK_CLOEXEC` atomically. Darwin SHALL set both flags with `fcntl` immediately after
`socket` and before publication, without promising cross-thread fork safety during that
single-threaded setup window. TCP SHALL disable positive `SO_LINGER`, retain Nagle by default, and
set `TCP_NODELAY` only when the explicit `noDelay` option requests it. Darwin SHALL suppress SIGPIPE
with `SO_NOSIGPIPE`; GNU writes SHALL use `MSG_NOSIGNAL`. The provider SHALL NOT change a process-wide
signal disposition, bind a local address, or enable `SO_REUSEPORT`.

#### Scenario: Configure a default TCP socket

- **WHEN** an admitted TCP candidate is opened with default options
- **THEN** it is nonblocking and close-on-exec, has positive linger disabled, retains Nagle, and has
  per-socket or per-send SIGPIPE suppression before the callback can observe it

#### Scenario: Enable no-delay explicitly

- **WHEN** `noDelay` is true
- **THEN** the provider enables `TCP_NODELAY` without changing reuse, bind, linger, or signal policy

### Requirement: Nonblocking connect proves completion and replaces failed descriptors

A connect that returns `EINPROGRESS` SHALL poll zero-time writable and error readiness and SHALL read
`SO_ERROR` before publishing `Open`; writable readiness alone SHALL never prove connection success.
A failed completion SHALL invalidate and close that descriptor before another candidate uses a new
one. A connect interrupted with `EINTR` SHALL close the provisional descriptor, perform a positive
cooperative wait, and issue a fresh counted attempt on a new descriptor rather than repeat connect
on the old descriptor.

On GNU, `EAGAIN` from pathname Unix connect SHALL mean full backlog, not pending connection: the
provider SHALL close the descriptor, wait positively under the unchanged deadline, and retry with a
fresh counted descriptor. Later writable readiness and `SO_ERROR == 0` for the rejected descriptor
MUST NOT publish `Open`. Unix paths SHALL be pathname-only, with a maximum `sun_path` capacity of
104 bytes on Darwin and 108 bytes on GNU including the terminating NUL; abstract namespace paths
SHALL be rejected and a connection SHALL never unlink its pathname.

#### Scenario: Confirm an in-progress TCP connect

- **WHEN** zero-time polling reports a pending socket writable
- **THEN** the provider publishes `Open` only after `SO_ERROR` reports success

#### Scenario: Replace an interrupted connect descriptor

- **WHEN** native connect returns `EINTR`
- **THEN** the provisional descriptor is closed once, one positive wait occurs, and a new counted
  attempt uses a different descriptor

#### Scenario: Reject false GNU Unix completion

- **WHEN** a full Unix backlog returns `EAGAIN` and the old descriptor later appears writable with
  `SO_ERROR == 0`
- **THEN** that descriptor never becomes `Open`; only a fresh successful connect may invoke the callback

#### Scenario: Reject an unsupported Unix path

- **WHEN** a path is empty, relative, contains NUL, exceeds the target `sun_path` capacity including
  its terminator, or requests the GNU abstract namespace
- **THEN** acquisition reports `InvalidEndpoint` without opening or unlinking a filesystem entry

### Requirement: Readiness waits are cooperative and deadline checked

Every descriptor SHALL remain nonblocking. An operation SHALL attempt its syscall once, inspect
relevant readiness with `poll` timeout zero after `EAGAIN` or pending connect, and then, when not
ready, call the active `MonotonicClock.waitUntil` for the earlier of the checked
`now + pollInterval` and the supplied overall deadline. The provider SHALL check a supplied deadline
before native I/O and after every resumed wait; equality SHALL be `Timeout`. It SHALL compute the
next wake with checked split-field arithmetic and return `TimeRangeError` if no finite future
`Instant` is representable, never overflow before taking the deadline minimum.

For a supplied deadline, the provider SHALL resample the active clock immediately before every
subsequent native work boundary: socket creation, each descriptor or socket-option setup call,
connect, readiness poll, `SO_ERROR` inspection, transfer syscall, and shutdown. If the deadline is
reached between two such boundaries, the later boundary SHALL not run.

Every unready result, `EINTR`, and ready-but-`EAGAIN` race SHALL take a positive clock wait before
retrying, so no path spins. Under `LocalScheduler`, the existing task-local clock replacement SHALL
park only the current task and permit siblings to advance. Outside a scheduler, the selected clock
owns blocking behavior and there SHALL be no cross-thread cancellation guarantee. At most one
readiness poll per interval while idle is promised; event-reactor latency and high-connection-count
throughput are not promised.

#### Scenario: Park an unready operation

- **WHEN** zero-time polling finds no readiness before a future deadline
- **THEN** the operation performs one positive clock wait and retries only after that wait resumes

#### Scenario: Time out before further I/O

- **WHEN** the active clock reaches the absolute deadline during a wait
- **THEN** the operation returns `Timeout` before another socket syscall or readiness poll

#### Scenario: Expire between native boundaries

- **WHEN** a sequenced clock reaches the absolute deadline after one native boundary completes
- **THEN** the provider returns `Timeout` before issuing the next setup, poll, `SO_ERROR`, transfer,
  or shutdown call

#### Scenario: Cancel at the clock wait

- **WHEN** structured cancellation destroys a scheduled operation parked in `waitUntil`
- **THEN** the timer guard and Wake are released, the connection closes once, and no kernel poll
  registration or successful transfer remains

#### Scenario: Reject unrepresentable wake arithmetic

- **WHEN** adding the positive interval to the current mark cannot produce a finite canonical Instant
- **THEN** the operation returns `TimeRangeError` without overflow, native retry, or clock wait

### Requirement: Reads preserve positive-prefix and EOF semantics

An empty `readSome` SHALL perform no native I/O. A nonempty read SHALL return only the exact positive
initialized prefix from `recv`; zero from `recv` SHALL return sticky `ReadTransfer.End`.
`EAGAIN` and `EINTR` SHALL follow the cooperative yielding path. `POLLHUP` SHALL NOT become End until
all readable bytes have been drained. `POLLERR` SHALL be reported from the actual `SO_ERROR` or
failed operation, and `POLLNVAL` SHALL invalidate the descriptor. Structured cancellation SHALL
never be recoded as EOF.

#### Scenario: Drain bytes before hangup EOF

- **WHEN** readiness reports readable data and `POLLHUP` together
- **THEN** reads return every available positive prefix before a later read returns End

#### Scenario: Keep an empty read local

- **WHEN** the caller supplies an empty output slice
- **THEN** the operation returns zero data without recv, poll, clock sampling, or EOF

#### Scenario: Invalidate an unusable descriptor

- **WHEN** readiness reports `POLLNVAL`
- **THEN** the connection becomes terminal and the read reports a typed provider failure

### Requirement: Writes preserve exact acceptance and directional closure

An empty `writeSome` SHALL perform no native I/O. Each nonempty send SHALL cap its length to the
selected platform's `ssize_t` maximum before ABI conversion and SHALL report only the exact positive
accepted prefix; accepted bytes SHALL never be replayed. `EAGAIN`, `EINTR`, and ready-but-`EAGAIN`
SHALL follow the cooperative yielding path. A successful zero count for nonempty input or any count
outside the offered capped prefix SHALL invalidate the connection and report
`InvalidTransferCount`. Socket `flush` SHALL be a no-op meaning only that the kernel accepted prior
bytes, not that the peer received them. `shutdownWrite` SHALL flush, call `shutdown(SHUT_WR)` once,
move to `WriteClosed`, reject later writes, retain reads, and remain idempotent.

#### Scenario: Accept one capped positive prefix

- **WHEN** a caller offers more bytes than one native transfer can represent and send accepts a
  shorter positive prefix
- **THEN** only the capped length is passed to native code and exactly the accepted prefix is reported

#### Scenario: Reject zero success for nonempty output

- **WHEN** native send reports successful zero for a nonempty offered prefix
- **THEN** the connection is invalidated and returns `InvalidTransferCount` without replaying bytes

#### Scenario: Shut down only writes

- **WHEN** `shutdownWrite` succeeds and is called again
- **THEN** exactly one `SHUT_WR` occurs, later writes report `WriteClosed`, and reads remain available

### Requirement: Terminal cleanup never waits or reuses a descriptor number

Terminal cleanup SHALL first release any active timer guard, then make the connection state and
descriptor unusable, and finally issue at most one nonparking native close. It SHALL never flush,
poll, wait for the peer, or retry close. GNU SHALL call `close` once even when it returns `EINTR`.
Darwin SHALL call the exact `close$NOCANCEL` symbol once after local invalidation and treat
`EINTR` or `EINPROGRESS` as a terminal in-progress disposition; only another error from an explicit
close MAY be reported. Cleanup close failure SHALL NOT replace an earlier success, typed failure, or
cancellation protected by the scope. Capacity or setup failure before publication SHALL release
every provisional descriptor by the same exact-once rule.

#### Scenario: Do not retry an interrupted close

- **WHEN** GNU `close` returns `EINTR` or Darwin `close$NOCANCEL` returns `EINTR` or `EINPROGRESS`
- **THEN** local ownership is already invalidated and the numeric descriptor is never closed again

#### Scenario: Preserve the protected failure

- **WHEN** scoped work has already failed and terminal close reports another error
- **THEN** the original failure remains observable and exactly one close attempt is recorded

### Requirement: Native failures remain typed at their owning boundary

Native acquisition and explicit connection operations SHALL distinguish `AttemptsExhausted`,
`TimeRangeError`, `InvalidEndpoint`, `FamilyUnsupported`, `PermissionDenied`,
`ConnectionRefused`, `NetworkUnreachable`, `AddressUnavailable`, `SystemResources`, `Timeout`,
`Closed`, `WriteClosed`, `InvalidTransferCount`, and `NativeFailure(operation, errno)` without
throwing or exposing an unknown error. Once supplied as `ByteDuplex`, the native provider SHALL map
deadline, count, and closure failures to the matching closed `ByteIoError` variants and every other
native failure to `ByteIoError.Provider` carrying the exact `ByteIoOperation` and `i32` provider
code. Structured cancellation SHALL remain structured cancellation.

#### Scenario: Preserve a rich acquisition failure

- **WHEN** native connect fails with permission denied before publication
- **THEN** acquisition reports `PermissionDenied` and closes the provisional descriptor once

#### Scenario: Map a provider operation failure

- **WHEN** an open connection's native read or write fails with an otherwise mapped errno
- **THEN** its ByteDuplex boundary reports `Provider` with the corresponding operation and stable
  `i32` code rather than widening the error channel

### Requirement: Native evidence pins ABI and lifecycle without network dependence

Independent Darwin and GNU C witnesses SHALL pin the declarations, signatures, symbols, constants,
sockaddr family fields, lengths, alignments, offsets, and pathname capacities used by `socket`,
`connect`, `getsockopt`, `setsockopt`, `fcntl`, `recv`, `send`, `shutdown`, `close`, and `poll`.
Deterministic source and C fixtures SHALL cover option setup, partial transfer, hangup draining,
shutdown, close, connect completion, interrupt retries, GNU full-backlog correction, deadlines,
attempt bounds, cancellation, and exact ownership in the shared native acceptance corpus without
calling external DNS or relying on a public network. The actor SHALL be registered and embedded
with executable reference examples that state supported profiles, Unix-path rules, `None` deadline
behavior, timer-poll latency/throughput, kernel-acceptance flush semantics, and structured cleanup
limits.

#### Scenario: Verify selected ABI and behavior

- **WHEN** focused analysis, target C witnesses, ownership checks, shared native corpus profiles,
  and documentation examples run
- **THEN** the actor's imports, layouts, retry/deadline paths, transfer ownership, and exact-once
  release agree on both admitted OS families without external network access

#### Scenario: Exclude broader networking policy

- **WHEN** the delivered surface is inspected
- **THEN** it contains no DNS, TLS policy, Happy Eyeballs race, abstract Unix namespace, pathname
  unlink, local bind, general reactor, worker thread, cross-thread Wake, or second socket provider
