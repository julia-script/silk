## Purpose

Define finite local HTTP/1 connection pooling whose capacity, security context, deadlines, reuse,
draining, cancellation, and closure remain explicit under affine ownership.

## ADDED Requirements

### Requirement: Pool configuration and context are finite and immutable

The standard library SHALL provide a bounded HTTP connection-pool actor whose configuration fixes
`maxTotal`, `maxIdle`, `maxPerOrigin`, and a finite positive idle timeout before publication. It
SHALL accept `maxTotal` from 1 through 1024, `maxIdle` from zero through `maxTotal`, and
`maxPerOrigin` from 1 through `maxTotal`; every other value SHALL fail `InvalidConfig` without
publishing a pool. Defaults SHALL be `maxTotal = 32`, `maxIdle = 16`, `maxPerOrigin = 8`, and an idle
timeout of 30 seconds. The pool SHALL preallocate bounded slot and accounting storage before
publication, and its shared state transitions SHALL NOT allocate.

One pool SHALL retain one immutable acquisition and security context. The context SHALL fix the
provider representation and identity, HTTP version and limits, TLS verification and ALPN profile,
original-origin admission, routing policy, proxy credential identity when present, and a frozen
prepared trust snapshot. A request SHALL NOT replace these values. An incompatible origin, route,
version, credential, trust, or provider context SHALL fail `ContextMismatch` before contact rather
than sharing an entry.

#### Scenario: Invalid capacity publishes nothing

- **WHEN** construction receives zero total capacity, excess per-origin capacity, excess idle
  capacity, or a nonpositive idle timeout
- **THEN** it returns `InvalidConfig` before publishing a pool or acquiring a connection

#### Scenario: Mutable trust source cannot change an existing pool

- **WHEN** the backing trust source changes after a pool has frozen its prepared snapshot
- **THEN** later connection openings use bounded copies of the frozen snapshot, while only a newly
  constructed pool can observe the changed source

### Requirement: Pool ownership and handles are scoped and affine

Pool construction SHALL occur through a nonparking scoped bracket whose finalizer marks shared
state closed, extracts idle connection owners, and physically closes them outside shared access.
The pool SHALL own every idle complete connection. A checked-out scoped lease SHALL exclusively own
one connection until publication or discard, and callback code SHALL receive only a nonescaping
exclusive borrow. Dropping an ordinary aggregate SHALL NOT substitute for effectful pool closure.

Creating another local handle SHALL be an explicit fallible effect that boundedly copies the frozen
context before publishing a shared-state alias. A failed copy SHALL publish no alias or partial
context. Callers SHALL NOT construct a handle by pairing arbitrary context with existing shared
state. Handles SHALL be local to one scheduler thread; the actor SHALL make no thread-safe,
cross-thread, atomic, or fairness claim.

#### Scenario: Lease authority cannot escape

- **WHEN** a callback attempts to return or duplicate its borrowed lease or connection
- **THEN** ownership analysis rejects the program without weakening the callback's generic result,
  error, or requirement channels

#### Scenario: Handle copy fails atomically

- **WHEN** bounded context or trust-snapshot copying fails while creating a handle
- **THEN** the original handle remains valid and no new shared alias is published

### Requirement: Capacity reservation and idle selection are atomic and fail-fast

`CONNECTING + LEASED + IDLE` SHALL never exceed `maxTotal`, and entries for one normalized original
origin SHALL never exceed `maxPerOrigin`. The origin key SHALL use scheme, validated original host
identity, and normalized effective port before route selection. Acquisition SHALL sample the
monotonic clock outside shared access, atomically select a compatible nonexpired idle owner or
reserve exactly one opening slot, and move the idle owner or reservation out before any network,
allocator, clock, or caller/provider callback.

Among compatible idle owners, acquisition SHALL select the most recently released. When global idle
eviction is needed, it SHALL select the least recently released and use stable insertion sequence
to break equal-timestamp ties. A full per-origin count MAY be relieved only by evicting an idle
entry for that same origin; unrelated global eviction SHALL NOT bypass it. If no legal reservation
or eviction exists, acquisition SHALL return `PoolFull` immediately without a waiter queue.
Counter arithmetic SHALL be checked before publication.

Idle expiry SHALL begin at successful idle publication and apply only while idle. Expiry SHALL be
lazy on acquisition, release, or explicit collection; it SHALL NOT use a background timer. Expired
and evicted owners SHALL be physically closed outside shared access.

#### Scenario: Concurrent opens stay within capacity

- **WHEN** interleaved fibers attempt more openings than the global or per-origin cap permits
- **THEN** reservations admit at most the configured count and every excess attempt fails
  `PoolFull` without contact

#### Scenario: Selection and eviction are deterministic

- **WHEN** multiple compatible idle entries and a separate oldest idle victim are available
- **THEN** checkout selects the most recently released compatible entry and global eviction selects
  the least recently released entry with stable sequence tie-breaking

### Requirement: Acquisition, request, handshake, drain, and idle time are separate

Each opening SHALL receive the current request's optional absolute deadline, optionally shortened by
an acquisition-only absolute deadline. TLS authentication SHALL additionally retain its finite
handshake-duration cap. A newly acquired owner SHALL be checked for expiry before publication.
Pooled HTTP owners SHALL retain no acquisition or previous-request deadline. Every exchange SHALL
receive its current request deadline unchanged through request body, trailers, response handling,
and any explicit drain; a later checkout SHALL NOT inherit an earlier acquisition or request cap.

All pool timestamps and absolute deadlines SHALL use one unchanged monotonic-provider timeline. Idle
publication SHALL sample the successful-return timestamp on the protected normal path. Exceptional
finalizers SHALL NOT request the clock. `None` SHALL continue to mean explicitly unbounded waiting;
the pool SHALL NOT claim preemption of synchronous resolution, trust loading, or arbitrary callback
CPU work. Native hostname resolution with a finite deadline SHALL retain its existing
`UnsupportedDeadline` rejection before resolver dispatch, while supported numeric, Unix, and
scripted acquisitions SHALL honor the earlier acquisition/request deadline.

#### Scenario: Later request outlives earlier deadlines

- **WHEN** a reusable owner was opened under deadline `t=5`, completed request A under `t=10`, and
  is checked out for request B under `t=20`
- **THEN** B can perform I/O after `t=10` and before `t=20` because neither earlier deadline remains
  attached to the owner

#### Scenario: Expired opening is never published

- **WHEN** acquisition or TLS completes after the effective acquisition/request deadline
- **THEN** the new owner closes, its reservation is refunded once, and no idle or leased owner is
  published

### Requirement: Reuse requires complete HTTP ownership and persistence

An owner SHALL be eligible for idle publication only after a successful scoped exchange has
completed the request body and trailers, completed final-response framing, returned to the client's
Ready phase, retained no unsolicited input suffix or pending write data, satisfied both message
persistence rules, and remained below its finite request-count budget. The eligibility decision
SHALL use a client-owned read-only query that includes private request count and retained suffix.
The pool SHALL NOT duplicate or reset the client's state machine.

Incomplete or abandoned callbacks, exhausted request budgets, close-delimited bodies, HTTP upgrade,
CONNECT-transferred ownership, malformed framing, transport/TLS/content-decoder failure, pending or
unsolicited bytes, and nonpersistent HTTP/1.0 or HTTP/1.1 messages SHALL close rather than reuse.
HTTP/1.0 SHALL require explicit keep-alive; HTTP/1.1 SHALL reject `Connection: close`. Exactly one
exchange MAY be active per owner. An idle peer MAY become stale; a first-I/O stale failure SHALL
evict the owner and SHALL NOT automatically replay the request.

#### Scenario: Complete exchanges reuse one owner

- **WHEN** two serial requests fully complete on a persistent connection below its request budget
- **THEN** the second checkout uses the same owned connection without repeating acquisition or TLS

#### Scenario: Exhausted and tainted owners are evicted

- **WHEN** `maxRequests` is one and the first exchange succeeds, or an owner retains unsolicited
  suffix bytes, transfers an upgrade, or observes incomplete, close-delimited, malformed, transport,
  TLS, or decoder state
- **THEN** release physically closes that exhausted or tainted owner and the next checkout must
  acquire another owner

### Requirement: Explicit drain is bounded inside the live exchange

Default exchange finalization SHALL perform no hidden read, drain, flush, graceful network shutdown,
or parking operation. An incomplete callback SHALL therefore close and SHALL NOT later be drained
back into reusable state.

The client SHALL provide one explicit live-exchange drain operation with a required finite absolute
deadline and aggregate wire-byte limit. Its default byte limit SHALL be 65536 and its accepted
maximum SHALL be 1048576. It SHALL clamp the drain deadline to the current request deadline, account
for payload, framing, and trailers, complete the selected content decoder, and call response finish
only after verified completion. Cap, timeout, malformed framing, read, allocation, transport, or
decoder failure SHALL leave the connection ineligible and preserve its precise typed error. Cleanup
SHALL preserve the protected operation's result and SHALL NOT hide a best-effort read after
cancellation.

#### Scenario: Drain completion enables reuse

- **WHEN** callback code explicitly drains the remaining selected response within both finite bounds
- **THEN** framing, trailers, and decoder validation complete before the owner can become reusable

#### Scenario: Drain cap or failure closes

- **WHEN** aggregate wire bytes exceed the cap or drain reaches timeout, malformed framing,
  transport failure, or decoder failure
- **THEN** the original typed outcome is preserved and the owner closes without reuse

### Requirement: Reservation, release, and closure are cancellation-safe

Every reservation SHALL carry an exact-once local refund guard from atomic admission until a
connection owner is published. Acquisition failure, allocation failure, typed failure, defect, or
cancellation SHALL refund capacity exactly once and physically close any acquired resource outside
shared access. Successful exchange release SHALL sample time before idle publication and either
publish one eligible owner or close it. If the pool closes during acquisition, late publication
SHALL fail `PoolClosed`, close the new owner, and refund its reservation.

Explicit close and the construction bracket's finalizer SHALL synchronously mark the pool closed,
move all idle owners out, and close each exactly once without parking. New acquisitions and
surviving handles SHALL return `PoolClosed`. Existing leased connections SHALL remain usable until
their scopes end; close SHALL NOT revoke authority under a live borrow. Their eventual releases
SHALL close instead of republishing. No provider callback, allocator call, clock sample, suspension,
or nested aliased shared access SHALL occur while pool state is borrowed.

#### Scenario: Cancellation refunds exactly once

- **WHEN** cancellation occurs before, during, or after provider acquisition but before publication
- **THEN** capacity returns exactly once, every concrete resource closes exactly once, and the
  cancellation outcome is not replaced

#### Scenario: Close preserves live leases

- **WHEN** pool close races with one idle owner, one opening reservation, and one checked-out lease
- **THEN** idle closes immediately, late opening publication is rejected and closes, the lease stays
  valid until callback completion, and its release closes rather than returning idle

### Requirement: Connection identity is conservative and route-complete

The connection key SHALL include HTTP or HTTPS scheme, normalized validated original host, effective
port, exact route transport endpoint identity, route mode Direct, ForwardProxy, or ConnectTunnel,
configured proxy credential identity within the immutable context, and tunnel target authority.
Forward-proxy reuse SHALL remain per original origin. Unix routes SHALL additionally include exact
pathname and HTTP origin. DNS aliases, resolved addresses, certificates, or caller-provided mutable
version tokens SHALL NOT establish equivalence.

Direct TCP, Unix HTTP, and HTTPS pooling SHALL be available on supported native targets through one
owned acquisition path. Proxy route construction remains separately owned; when a sealed proxy
route key is unavailable or a target/route combination is unsupported, admission SHALL fail before
contact. Trust copying, resolver, socket, TLS, pool, client, source, and callback failures SHALL
remain precise alternatives rather than becoming unknown or string failures.

#### Scenario: Route or security mismatch cannot coalesce

- **WHEN** two requests differ in original origin, Unix path, proxy endpoint or mode, tunnel target,
  credential identity, trust snapshot, TLS verification profile, ALPN, or provider identity
- **THEN** they never share one connection entry

#### Scenario: Unsupported composition performs no contact

- **WHEN** a target or route cannot provide the requested owned acquisition contract
- **THEN** it returns its typed unsupported failure before resolver, socket, trust, or transport
  contact

### Requirement: Pooling evidence is bounded and portable

Delivery SHALL register and document the target-neutral actor and supported native composition.
One shared structured-analysis source SHALL prove public operations, exact generic rows, and
client/native seams; one consolidated frontend-negative source SHALL prove lease/connection escape
and duplication rejection. One compact table-driven scripted runtime corpus SHALL cover distinct
capacity, selection, expiry, deadline, reuse, drain, closure, failure, and cancellation signals. The
same source SHALL provide exactly one intended LLVM-to-Wasm portability leg for target-neutral pool
state behavior. Existing HTTP, proxy, TLS, trust, and native suites SHALL supply their established
protocol and transport vectors rather than being duplicated.

The change SHALL NOT add a per-case compilation matrix, new test worker, stress test, timing
assertion, fresh-process test, broad TLS/socket matrix, or tests whose sole purpose is reproducing a
full CI or release pipeline.

#### Scenario: One shared corpus proves target-neutral behavior

- **WHEN** the table-driven pooling source runs through native acceptance and its one named Wasm leg
- **THEN** both produce the same capacity, selection, expiry, deadline, drain, closure, and release
  outcomes without another compiled program per scenario
