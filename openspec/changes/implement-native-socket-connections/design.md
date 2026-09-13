## Context

See `proposal.md` for motivation and the two delta specs for the observable contract. The current
tree already supplies owned `Endpoint` values and bounded ordered resolution results in
`silk.network_address`/`silk.resolver`, exact-prefix `ByteDuplex`, generic
`Effect.useReleaseNonParking`, provider-replaceable `MonotonicClock`, and task-local scheduler
timers. `tls_connection.withClient` accepts an explicit concrete ByteDuplex provider, so the socket
scope must lend such a provider without returning descriptor ownership.

The compiler already centralizes native import/export/data/catalog spelling in
`ForeignSymbol.isValidSpelling`; declaration collection and ABI catalog ingestion both call it.
Only its identifier tail excludes `$`. The optional quoted `as` alias, foreign lowering, object
emission, and linker path already exist.

## Goals / Non-Goals

**Goals:**

- Make one source actor own every socket descriptor, native layout, errno decision, retry, and close.
- Keep connection, timer-wait, and caller-buffer ownership statically explicit across suspension.
- Reuse the delivered ByteDuplex, clock, scheduler, endpoint, and finalizer contracts without new
  compiler privilege.
- Keep evidence deterministic and consolidated at the cheapest layer that proves each claim.

**Non-Goals:**

- A reactor, epoll/kqueue provider, worker pool, threaded cancellation, cross-thread Wake, or
  high-connection-count throughput design.
- DNS, Happy Eyeballs, local binding, listener/accept, TLS policy, proxy policy, Unix abstract
  addresses, pathname unlinking, or a second socket provider.
- Extending source syntax or granting `$` to Silk identifiers; only quoted native-symbol values and
  ABI metadata use the widened spelling.

## Decisions

### 1. One selected `native_socket` actor owns acquisition and the concrete provider

`native_socket.silk` contains `ConnectOptions`, Unix-path admission, typed socket errors, private
target layouts/constants/declarations, an affine `Connection`, and the two scoped entry points:
`connectResolved(endpoints, options, deadline, callback)` and
`connectUnix(path, options, deadline, callback)`. Each callback receives only a higher-ranked
`&mut Connection`. `Connection` implements `ByteDuplex`, so callers can pass it directly to
`tls_connection.withClient`, `buffered_duplex.withBuffered`, or explicitly provide it to ordinary
ByteDuplex operations. Its fields and descriptor are private, and the callback lifetime prevents
the provider loan from escaping.

Both entry points acquire a connection, then run the caller under `Effect.useReleaseNonParking`.
The release callback terminally closes the same owned connection and recovers close failure so it
cannot replace the protected outcome. Callback requirements retain the existing
`Without<CallbackRequirements, ByteDuplex>` absence constraint: connection operations may bind the
private provider internally, while a second ambient ByteDuplex alias is rejected.

Returning an owned connection was rejected because it would move cleanup and cancellation
obligations onto every caller and would not fit the existing scoped TLS adapter. A new `Socket`
service was rejected because runtime replacement is not needed: `ByteDuplex` is the replaceable
contract and native acquisition remains an explicit constructor boundary.

### 2. Connection state separates descriptor ownership from public error policy

The private state pairs a descriptor with `Connecting | Open | WriteClosed | Closed`. Construction
creates `Connecting`; only verified connect completion transitions to `Open`; successful
`SHUT_WR` transitions to `WriteClosed`; every ambiguity or terminal operation moves to `Closed`
before native close. No Copy or Clone conformance is declared.

`ConnectError` owns input, deadline, attempt-exhaustion, and pre-publication failures.
`AttemptError` carries the nonrecursive last-candidate reason embedded by
`AttemptsExhausted { attempted, last }`. `NativeOperation` identifies the exact syscall for
`NativeFailure`. Direct connection policy retains the richer socket tags; the ByteDuplex
implementation maps Timeout, InvalidTransferCount, and closed direction to the matching
`ByteIoError` shape and other errno outcomes to `Provider { operation, code: errno }`. In
particular, a post-shutdown ByteDuplex write becomes its closed write operation because the generic
envelope has no `WriteClosed` tag.

One giant error containing arbitrary nested `ConnectError` was rejected because recursive
by-value unions would complicate representation. Erasing acquisition failures immediately into
`ByteIoError` was rejected because callers need to decide whether resolution candidates or policy
failed before a transport is published.

### 3. Target branches declare real libc ABI and nothing on unsupported profiles

The actor follows existing selected native-source modules. Darwin and GNU branches declare their
own `SockAddrV4`, `SockAddrV6`, `SockAddrUnix`, `PollFd`, option values, errno values, and function
signatures. Darwin uses its length-plus-u8-family sockaddr headers and 104-byte Unix path; GNU uses
its u16 family headers and 108-byte Unix path. `socklen_t`, poll-count, pointer, `size_t`, and
`ssize_t` differences remain target-specific source facts rather than a shared guessed record.

Each branch wraps `socket`, `connect`, `getsockopt`, `setsockopt`, `fcntl`, `poll`, `recv`, `send`,
`shutdown`, and close with conservative foreign contracts. GNU declares ordinary `close` and uses
atomic socket flags plus `MSG_NOSIGNAL`. Darwin declares
`closeSocket as "close$NOCANCEL"`, configures descriptor flags immediately with `fcntl`, and uses
`SO_NOSIGPIPE`. Positive linger is disabled through an explicit C-layout linger value on both.
Independent C witnesses, not copied source constants, assert layouts, constants, and callable
signatures.

A common sockaddr overlay was rejected because Darwin and GNU disagree in header layout. Raw
kernel syscalls and a generated compiler socket shim were rejected because the admitted contract is
libc-based ordinary source. An invented foreign link-name property was rejected because quoted
aliases are already the language's native-name mechanism.

### 4. The shared foreign-symbol rule admits `$` only after the leading identifier character

Revise `ForeignSymbol.isValidSpelling` from `[A-Za-z_][A-Za-z0-9_]*` to
`[A-Za-z_][A-Za-z0-9_$]*`. Declaration collection, data declarations, exports, ABI-manifest input,
conflict detection, and emission continue to use the same admitted string and existing reserved
checks. Focused generic tests cover `close$NOCANCEL` plus another dollar-bearing spelling, ABI JSON
round-trip, linked object fidelity, leading `$`, whitespace, NUL, non-ASCII, and reserved
negatives. No compiler branch recognizes `closeSocket`, `native_socket`, Darwin, or a socket type.

Allowing arbitrary non-NUL link names was rejected because whitespace and platform decoration make
catalog interoperability and diagnostics ambiguous. Allowing `$` only in the socket source by
special case was rejected because the symbol is an ABI value and every catalog owner must agree.

### 5. Acquisition is an explicit fresh-descriptor state machine

`ConnectOptions.make` validates `pollIntervalNanoseconds` and `maxAttempts`; `defaults` selects
1 ms and 64. Resolved input is validated before socket creation. The endpoint loop retains the
ordered slice and one attempt counter. Every call to native connect increments the counter. A
failure that policy permits to advance closes the current descriptor before moving to the next
candidate. An attempt-cap failure retains a small nonrecursive `AttemptError` value as its last
cause.

TCP setup creates a stream socket, applies platform nonblocking/CLOEXEC, positive-linger disable,
SIGPIPE suppression, and optional no-delay before connect can publish state. Endpoint conversion
copies bytes into the exact target sockaddr on the stack. IPv4 and IPv6 are the only resolved
families.

`EINPROGRESS` enters completion polling. A writable or error result always calls `getsockopt` for
`SO_ERROR`; only zero opens the connection. `EINTR` never repeats connect on the same descriptor:
it closes, waits one positive interval under the same deadline, and creates a fresh counted socket.
The GNU Unix `EAGAIN` branch is kept separate from `EINPROGRESS` and follows that same fresh retry.
Unix path validation copies an admitted absolute byte path and its NUL into fixed target storage;
the connection never retains or unlinks the caller path.

Reusing a descriptor after failed connect was rejected because its state is unspecified. Treating
GNU Unix `EAGAIN` as pending was rejected because a full listener backlog can later appear writable
with `SO_ERROR == 0` without that connect having succeeded. Parallel candidate racing was rejected
because it would require multiple owned attempts and readiness arbitration beyond this ticket.

### 6. Zero-time poll plus positive clock waits provides cooperative readiness

Each connect/read/write loop follows one pattern: check the absolute deadline, attempt the
nonblocking syscall once, use `poll(..., 0)` only to inspect immediate readiness, and when progress
is unavailable sample `MonotonicClock.now`, derive the earlier of the next interval and deadline,
then call `waitUntil`. After resume it checks the deadline again before native work.

A private checked-add helper works on canonical split seconds/nanoseconds. When a deadline exists,
the helper first compares remaining time with `pollInterval`, so it can choose the existing finite
deadline without attempting an overflowing addition. Without a deadline, or when the interval is
smaller, it uses checked integer conversions/additions and returns `TimeRangeError` instead of the
trap-based `MonotonicClock.deadlineAfter` helper. The validated interval is positive, so every
unready, EINTR, and ready-but-EAGAIN retry yields.

`poll` never remains registered when the clock wait begins. Under `LocalScheduler`, its lexical
clock installs a timer and parks only the task; cancellation removes that timer/Wake before the
outer socket finalizer closes the descriptor. With an OS clock, the wait may block the host thread,
which is already the provider's documented semantics.

A blocking `poll` timeout was rejected because it would stall the single-threaded scheduler and
could not be structurally canceled. A zero-time busy loop was rejected because readiness and signal
storms could starve peer tasks. A new reactor was rejected because it would create a second
readiness ownership system when bounded timer polling is sufficient for the first provider.

### 7. I/O loops acknowledge only native positive prefixes

Read and write empty slices return locally. Nonempty transfers cap the offered native length before
`size_t`/`ssize_t` conversion. Read maps a positive `recv` count to Data, zero to sticky End, and
keeps reading when HUP accompanies readable bytes. Write reports exactly the positive `send` count;
the safe ByteDuplex wrapper remains the outer count validator, while the provider also closes on an
impossible native success so the ambiguous buffer is never replayed. ERR obtains the real
`SO_ERROR`; NVAL terminalizes the descriptor.

Flush returns immediately because successful send means kernel acceptance only. Shutdown calls the
safe flush boundary, issues `SHUT_WR` once, and retains the read half. Repeated shutdown is local;
writes in `WriteClosed` fail without native work.

Looping until a complete caller buffer was rejected because ByteDuplex intentionally exposes
partial progress. Treating HUP as immediate EOF was rejected because it can discard already queued
bytes. Retrying an ambiguous buffer after zero or cancellation was rejected because the peer may
already have observed a prefix.

### 8. Cleanup invalidates first and never parks or retries close

Release first allows any nested clock wait's guard to finish cancellation, then changes phase to
Closed and extracts/invalidates the descriptor before one close call. It does not poll, flush,
shutdown, or await the peer. GNU ignores close EINTR for ownership purposes. Darwin's one
`close$NOCANCEL` call treats EINTR/EINPROGRESS as terminal in-progress disposition; other errors are
available only to explicit close, while scoped release recovers them. This prevents a numeric fd
that the kernel may already have reused from being closed twice.

Retrying close was rejected because POSIX implementations may release the descriptor before
reporting an error. Drop-only cleanup was rejected because the current fatal-trap boundary does not
promise Drop and because Effect structured cancellation already supplies the exact protected exit
semantics required here.

### 9. Evidence is partitioned by semantic layer

A focused socket test builds one shared analysis snapshot per distinct source/target program. It
checks public signatures, target selection, foreign inventories, typed mappings, static state
transitions, ownership rejection, and generic `$` diagnostics without compiling a native binary for
each claim. Ticket-local support exports one native corpus program plus Darwin/GNU C fixtures and
profile data; the coordinator adds only the shared corpus import/entry. The corpus tables setup,
connect, partial I/O, HUP drain, shutdown, backlog, interruption, and close cases in one compiled
program per profile. Scripted clock/readiness actors cover positive waits, deadline precedence,
attempt caps, checked arithmetic, and cancellation deterministically. C compile-time witnesses pin
the ABI separately. Reference examples use only local deterministic fixtures; no live network or
DNS is needed.

Per-feature ad hoc native-binary tests were rejected because `DriverNativeAcceptance` already owns
the differential execution boundary. Timing assertions were rejected because the contract is
logical-clock ordering, not wall-clock latency.

## Risks / Trade-offs

- **[Idle readiness is observed at most once per interval]** → Document the default 1 ms
  latency/CPU trade-off and bounded range; a future reactor is a separate clean design.
- **[Darwin flag setup is not atomic with socket creation]** → Perform `fcntl` immediately before
  publication and explicitly exclude cross-thread fork safety from this single-threaded provider.
- **[Platform constants or layouts drift]** → Keep Darwin/GNU declarations separate and gate them
  with independent C headers, offsets, sizes, constants, symbols, and calling signatures.
- **[A ready notification races with EAGAIN]** → Return to the positive timer-wait path; never spin
  or infer transferred bytes.
- **[Close reports an error after releasing the fd]** → Invalidate ownership before exactly one
  close call and never retry the number.
- **[Finite attempts are mistaken for finite elapsed time]** → State that `None` may wait forever
  on one pending attempt and that the cap counts syscalls rather than time.
- **[Timer polling is mistaken for cross-thread cancellation]** → Limit cancellation guarantees to
  structured same-thread scheduler waits and retain the selected clock's behavior elsewhere.

## Migration Plan

Land the generic symbol validator and socket actor with focused/support evidence and documentation,
then have the integration coordinator register `silk.native_socket`, regenerate the shared
standard-library catalog, add the reference index entry, and wire the exported corpus program into
the shared native harness. Existing ByteDuplex, TLS, endpoint, and clock APIs are unchanged.
Dependent client/listener tickets adopt the scoped provider directly; no compatibility adapter or
legacy descriptor path is retained. Before those dependents land, rollback removes the actor,
validator widening, fixtures, registrations, generated output, documentation, and this unarchived
change together.
