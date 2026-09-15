## Context

See `proposal.md` for motivation and
`specs/bounded-http-connection-pooling/spec.md` for the delivery contract. The streaming client
already owns affine `Connection<P>` values, returns them to Ready only after a successful reusable
exchange, retains unread suffixes, counts requests privately, and closes terminal transports
idempotently. Native acquisition currently hides its owned connection behind callbacks, and an
owner can retain the acquisition call's overall deadline. TLS authentication already consumes an
owned trust snapshot and produces a complete owned transport; trust snapshots support bounded
copying. `Shared<T>` supplies local callback-scoped synchronous access but forbids aliased nested
access and must not be held across allocation, suspension, or external callbacks.

JUL-199 owns proxy route construction and provides sealed route identity. Direct TCP, Unix, and TLS
pooling can ship independently; proxy integration consumes that identity when present. JUL-200
builds the fetch facade over this pool and is blocked by this change.

## Goals / Non-Goals

**Goals:**

- Own finite idle HTTP connections and opening/leased accounting behind one local shared state.
- Preserve affine provider and connection ownership through callback-scoped checkout and exact-once
  cleanup under every protected exit.
- Freeze trust and all transport/security identity at construction while keeping request
  authorization outside pooled state.
- Separate opening, request, handshake, drain, and idle deadlines without adding a deadline setter.
- Compose existing HTTP/TLS/proxy owners and error rows instead of duplicating protocol state.
- Keep verification to shared semantic sources and one compact target-neutral runtime program.

**Non-Goals:**

- Thread-safe access, atomics, mutexes, cross-thread wakeups, waiter queues, fairness, or background
  expiry work.
- Automatic retry or request replay, pipelining, HTTP/2, origin coalescing, WebSocket pooling, or a
  general Stream abstraction.
- Trust refresh inside one pool, per-request TLS/ALPN replacement, ambient credential lookup, or
  arbitrary caller-supplied connection owners.
- Proxy transport construction before JUL-199 is available or a new compiler intrinsic.

## Decisions

### The pool actor separates immutable handle context from mutable shared slots

Add one ordinary-source `silk.http_connection_pool` actor. A `Pool` owner and every fallibly copied
`Handle` contain an owned immutable context beside a `Shared<PoolState<P>>` alias. Only the actor's
constructors can pair these values. The context retains admitted config, provider identity, HTTP
limits/version, route/security profile, and frozen trust. Copying a handle first copies all bounded
context, including `TrustSnapshot.copy`, and only then clones the Shared alias.

`PoolState` owns preallocated slots and scalar counts. A slot is Vacant, Idle with a complete
`Connection<P>`, or reserved metadata for an opening/lease accounting transition; checked-out and
opening concrete resources live outside Shared. Each idle slot records the conservative connection
key, returned timestamp, and monotonically checked insertion sequence. Counts are derived or
updated with checked arithmetic under one `Shared.withMut` call.

Alternative: retain context inside `Shared<PoolState>`. Rejected because snapshot copying and
validation can allocate, and borrowing it through Shared across those effects would violate the
shared-access contract. Alternative: expose raw constructors. Rejected because an arbitrary context
could be paired with unrelated idle owners.

### A higher-ranked connector opens complete owners

Define a target-neutral compile-time connector interface whose scoped callback receives one newly
owned complete `Connection<P>` or whose operation returns it directly when Silk ownership permits.
Inputs include the admitted connection key, current request deadline, optional acquisition-only
deadline, and immutable context view. The connector preserves concrete provider type and generic
acquisition error/requirement rows. It never receives shared pool state.

Add the smallest native owned-acquisition operation beside `http_client_native`. It reuses current
resolver, socket, trust, TLS, ALPN, and HTTP allocation owners under one release guard armed from
socket acquisition through publication. The pool provides a bounded copy of its frozen snapshot to
each HTTPS authentication. The resulting HTTP owner is constructed with `overallDeadline = None`;
opening and handshake limits exist only in the acquisition call.

Proxy-aware connectors later use JUL-199's sealed route and route key. Unsupported route modes fail
before opening. No connection is extracted from an existing `withConnection` callback, because that
callback's finalizer owns closure.

Alternative: let the pool call native sockets directly. Rejected because scripted providers and
future fetch composition need a target-neutral seam. Alternative: accept arbitrary caller-created
`Connection<P>` values. Rejected because their immutable acquisition and security context cannot be
proven.

### Checkout is a three-phase reservation bracket

Checkout follows three phases:

1. Sample current monotonic time outside Shared. Under one synchronous mutation, lazily remove
   expired entries, choose the newest compatible idle owner, or reserve one opening slot after
   deterministic legal eviction. Move selected owners, victims, and the reservation token out.
2. Outside Shared, close victims, open if needed, and invoke a higher-ranked use callback with an
   exclusive connection borrow. A local nonparking guard owns either the reservation or concrete
   connection throughout acquisition and use.
3. On successful return, sample the return time, query client-owned eligibility, then reenter Shared
   once to publish an eligible idle owner if capacity and open state still allow it. Otherwise move
   the owner out and close. On failure, defect, or cancellation, close/refund without sampling time.

The reservation token contains enough accounting identity to refund exactly once but no Shared
borrow. Intrinsic replacement arms/disarms state transitions without a fallible gap. Pool closure
sets `closed` before extracting idle owners, so a concurrent late publication is rejected. A live
lease remains outside state and closes on release after closure.

Alternative: hold `Shared.withMut` across opening or callback use. Rejected because it permits
parking, reentrancy traps, and allocator/provider effects under shared access. Alternative: a waiter
queue. Rejected because the accepted admission policy is immediate fail-fast.

### Capacity keys and ordering are conservative

The key is a fully owned admitted value: original scheme/host/effective port, exact route endpoint
and mode, proxy credential identity, tunnel authority, Unix path when present, and immutable context
identity. DNS names are ASCII-casefolded, numeric families retain exact network bytes, default ports
normalize, and forward proxy connections remain partitioned by original origin. Provider and
security identity are constructor-sealed, never inferred from a mutable caller hash.

The mutation step first purges expired idle candidates, then prefers the newest compatible idle
entry. Opening reservation checks the per-origin total before any global eviction; only an idle
entry of that origin can relieve that cap. Global pressure evicts the oldest idle entry, using an
insertion sequence for equal timestamps. `maxIdle = 0` remains valid and causes every returned owner
to close.

Alternative: key only by resolved endpoint or certificate. Rejected because it could coalesce
different HTTP authority, proxy, credential, or verification contexts. Alternative: share one
forward-proxy socket across origins. Rejected in the initial actor in favor of conservative policy.

### Reuse eligibility remains owned by the HTTP client

Add a narrow synchronous read-only client operation such as `reuseEligibility(&Connection<P>) ->
ReuseEligibility`. Eligibility includes Ready phase, no retained suffix, no pending writes, positive
message persistence, no upgrade/tunnel transfer, and `requests < maxRequests`. It does not mutate,
reset, or expose private counters. The pool treats only Eligible as publishable and physically
closes every other result outside Shared.

`withExchange` continues to receive a request-local absolute deadline. Pooled owners have no overall
deadline, so the existing clamp preserves exactly the current request bound. This avoids a public
deadline setter and prevents acquisition or prior-request caps from leaking into reuse.

Alternative: check only `ConnectionPhase.Ready`. Rejected because Ready alone does not expose the
request budget or prove the retained suffix empty. Alternative: duplicate the client's fields in
the pool. Rejected because it creates a second state machine and can drift from protocol ownership.

### Explicit drain extends the live exchange, not release cleanup

Add a client/content-owned operation such as `drainAndFinishAtMost(exchange, maxWireBytes,
deadline)`. It is callable only while the response exchange is live, requires a finite deadline,
clamps it to the request deadline, and drives the existing content/framing decoder through terminal
trailers and decoder validation before `finishResponse`. Aggregate wire counting stays with the
client because parser framing counters are private. Cap and all typed failures leave the exchange
nonreusable.

Pool release never calls this operation. Default abandoned-exchange behavior simply allows the
existing bracket to mark the owner closed, then physically closes it without network I/O. This
preserves cancellation and callback failures rather than replacing them with cleanup reads.

Alternative: reuse `discardRemaining` after the callback exits. Rejected because selected content
and live decoder state are no longer safely available and cleanup could park. Alternative: count
decoded bytes in the pool. Rejected because framing and trailers are part of the accepted wire cap.

### Pool closure is an explicit nonparking state transition

Construction uses a scoped nonparking bracket. Explicit `close` and bracket finalization share one
transition: mark state closed, move idle owners into preallocated output storage, return from Shared,
then close owners exactly once. New checkout and late publication return `PoolClosed`. Opening and
lease guards retain Shared accounting until they refund, so pool-state lifetime naturally extends
past the root owner's close without invalidating a caller borrow. No `finishAll` or force-close of
live leases is introduced.

Alternative: close transports underneath existing leases. Rejected because it revokes affine
authority already lent to user code. Alternative: rely on aggregate drop. Rejected because physical
transport close is effectful and must have a typed owner.

### Evidence shares expensive compiler boundaries

Add pool declarations and exact generic-row witnesses to one existing HTTP positive Analysis source
and consolidate deliberate lease/connection escape and duplication programs into one frontend-only
negative source. Add one table-driven pool scenario to the shared native corpus and reuse that exact
source for one named LLVM-to-Wasm leg. The table covers only distinct capacity, ordering, expiry,
deadline separation, reuse, drain, closure, typed failure, and cancellation signals. Reuse one
existing HTTPS/trust vector to prove frozen snapshots and handshake-deadline nonretention; native
preflight stays structural. Do not create a worker, per-case compiler realization, stress/timing
matrix, fresh-process test, new socket/TLS matrix, or local full-pipeline gate.

## Risks / Trade-offs

- [A shared callback accidentally encloses allocation or suspension] -> make every shared callback
  a pure synchronous slot transition that returns owned work; assert structural call boundaries and
  runtime reentrancy behavior.
- [Cancellation loses a reservation or replaces the protected outcome] -> arm one local state guard
  before leaving admission and use protected nonparking finalization for every acquisition/use path.
- [An incomplete connection is published because Ready is treated as sufficient] -> centralize a
  positive client eligibility query that includes suffix, persistence, transfer, and request budget.
- [Security contexts alias mutable caller state] -> privately construct contexts, own all identity
  material, freeze trust once, and make handle copying fallible before Shared alias publication.
- [Deterministic eviction becomes expensive] -> scan the bounded preallocated slot array; the hard
  maximum of 1024 keeps work finite without adding an allocating index.
- [A stale idle peer fails on checkout] -> evict and preserve the first I/O failure; never retry the
  request because replay authority belongs above the pool.
- [Pooling tests amplify compiler memory] -> one positive Analysis source, one negative frontend
  source, one shared corpus program, one reused TLS vector, and one named Wasm leg.

## Migration Plan

This is a green-field actor. Add its client/native seams, registration, generated surfaces,
documentation, and evidence together, then update the future fetch actor to use the final API
directly. Rollback removes the pool actor and its narrow seams; no persisted pool data, legacy API,
compatibility path, or migration shim exists.
