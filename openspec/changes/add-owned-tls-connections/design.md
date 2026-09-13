## Context

See `proposal.md` for motivation. The current `tls_connection.Connection<'transport, P>` moves
`ConnectionState` into a callback object but stores only `&'transport mut P`; `withClient` then
terminally closes `P` on every callback exit. The TLS client itself already owns its trust snapshot,
copied reference identity and ALPN bytes, traffic secrets, transcript, input, output, authentication
evidence, and validation instant. The missing piece is ownership of the exact provider beside that
state.

`Effect.useReleaseNonParking` and `ByteDuplex.close` already provide cancellation-safe nonparking
release. Native socket acquisition already builds an owned `Connection` internally, and native
descriptors invalidate their numeric handle before close. The public native entry points currently
hide those owners inside callback scopes.

This change establishes only the native/TLS ownership prerequisite. JUL-23 owns the later HTTP
connection and exchange boundary. JUL-197 owns pooling after JUL-23 has supplied that contract.

## Goals / Non-Goals

**Goals:**

- Publish one authenticated affine value that owns TLS state and its exact provider.
- Make trust preparation, provider transfer, pre-publication authentication, borrowing, and
  terminal release have explicit ownership and error boundaries.
- Reuse the existing TLS pump and native connection state rather than adding parallel protocol or
  syscall implementations.
- Preserve generic Effect success, failure, and requirement channels through scoped convenience.
- Fix the external absolute handshake-deadline seam required by JUL-23.

**Non-Goals:**

- HTTP connection owners, owned buffers, head/body/trailer views, exchange completion, reusable
  evidence, or any placeholder API for those concepts.
- Pool reservations, active return, trust-refresh eviction, idle ordering, expiration, capacity,
  wait queues, coalescing, pool close, or pool synchronization.
- Outer origin, resolved-route, proxy, credential, or security-context identity.
- TLS resumption, early data, mutable per-request TLS policy, or authentication against a resolved
  address rather than the TLS client's original reference identity.
- A general asynchronous destructor, new synchronization primitive, compiler-known TLS/socket/HTTP
  actor, or cleanup after fatal traps.
- Changing the transport-independent TLS 1.3 protocol profile or ByteDuplex transfer semantics.

## Decisions

### 1. Make the transport-bearing owner authoritative

Introduce the source-owned shape:

```silk
pub struct OwnedConnection<P> {
  state: ConnectionState
  provider: P
  provenance: AuthenticatedProvenance
}
```

All fields remain private and the type has no `Copy` or `Clone`. `ConnectionState` continues to own
the existing TLS `Client` and scratch/cursor/phase data. `AuthenticatedProvenance` records only
facts established by this TLS client: its original reference identity, fixed offered and negotiated
ALPN evidence, authentication evidence and validation instant, and association with the provider
that carried the handshake. It contains no outer origin-admission, route, proxy, credential,
security-context, or pool identity. Borrowed accessors may expose TLS facts, but no public
constructor or setter can forge them.

Connection operations take `&mut OwnedConnection<P>`, split-reborrow its private state and provider,
and bind that provider only around the existing transport-driving helpers. Their service rows
exclude ambient `ByteDuplex`. The old borrowed-provider `withClient(&mut P, ...)` shape is deleted.
The retained convenience consumes `P`, calls the authoritative owned constructor, and protects the
owner with `Effect.useReleaseNonParking`.

Keeping both owned and borrowed TLS drivers was rejected because their phase, deadline, progress,
and invalidation rules could diverge. Storing `ConnectionState` and `P` separately was rejected
because a caller could substitute or independently close the provider.

### 2. Make trust preparation a caller-owned step before provider transfer

The breaking public boundary becomes:

```silk
pub effect fn authenticateOwned<P>(
  provider: P,
  config: &ClientConfig,
  trust: TrustSnapshot,
  options: ConnectionOptions,
) -> OwnedConnection<P>
! ConnectionError | OutOfMemoryError
? &mut SystemClock | &mut MonotonicClock | &mut Allocator | &mut Random
```

`ConnectionOptions` retains only handshake policy, including its finite duration and optional
external absolute deadline. Remove its `trust: TrustLoadLimits` field and the now-unused
`TrustLoadLimits` import. Remove the `TrustSource` import and lexical requirement, and remove the
`TrustSourceError` import and failure alternative from `authenticateOwned`, `withClient`, their
helpers, callers, examples, and public reference signatures.

An application that needs a fresh snapshot explicitly runs `TrustSource.load`,
`TrustSnapshot.copy`, `TrustSnapshot.combine`, or decoding before calling the TLS operation.
Preparation owns its partial allocations and reports its existing `TrustSourceError` result and/or
`OutOfMemoryError`. Failure releases partial output; a copy leaves its original snapshot unchanged,
and a load leaves the source service with the caller. Preparation has not transferred `P`, so TLS
authentication promises no provider cleanup for that pre-call failure. The prepared
`TrustSnapshot` is a unique affine input. After invocation, the TLS client consumes it; construction
failure releases it through ordinary frame ownership, and successful `OwnedConnection` retains it
inside `Client` for the session.

Loading trust internally was rejected because it conflates application trust selection with
network-resource acquisition and expands every TLS call's service and error rows. Borrowing a
snapshot through a suspending handshake was rejected because the TLS client requires an independent
owner and borrowed state must not cross Effect boundaries.

### 3. Guard the provider only after both owned inputs arrive

On function entry, after the provider and already prepared snapshot have been received,
`authenticateOwned` immediately moves `P` into a private guard before time sampling, client
construction, allocation, or transport work. The guard has `Acquiring`, `Ready`, and `Taken` states;
only the module moves a fully authenticated `OwnedConnection<P>` through `Ready -> Taken`.

The guard's nonparking release terminally closes its retained provider unless ownership was taken.
The snapshot, client, and allocations follow ordinary affine frame cleanup. Typed failure and
structured cancellation/interruption close the provider once and preserve the protected exit;
fatal traps intentionally carry no cleanup guarantee. No raw constructor accepts caller-created
state, authentication metadata, or provenance.

Returning state before authentication was rejected because a caller could send application bytes
before peer authentication. Guarding the provider while an external trust load is still running was
rejected because trust preparation is not part of this function and the provider remains with the
caller until invocation.

### 4. Clamp one handshake duration to the caller's external absolute deadline

`ConnectionOptions` retains the finite handshake duration and adds
`externalDeadline: Option<Instant>`. After client construction, `authenticateOwned` samples the
active monotonic clock immediately before the first ClientHello transport boundary, computes the
duration deadline, and selects the earlier present deadline. It checks that mark before output,
after each suspended boundary, between driver steps, and immediately before `Ready` publication.

The external mark is never converted to a remaining duration and restarted. This makes JUL-23's
eventual overall deadline composable. Caller-side trust preparation remains outside the handshake
duration and provider guard; an external deadline may expire during that preparation and is checked
before the first transport byte after invocation.

Replacing the duration with only an absolute deadline was rejected because standalone TLS callers
still need a finite default. Renewing a duration after each partial operation was rejected because
fragmented peers could extend the handshake indefinitely.

### 5. Arm every suspending direct-owned operation against ambiguous cancellation

Every public suspending `readSome`, `writeSome`, `flush`, and `shutdownWrite` operation builds an
internal operation guard that exclusively borrows `OwnedConnection<P>` before the first provider
call. The guard begins `Armed`. The protected body may change it to `Completed` only after it has
committed exact returned progress, or after a returned typed failure has made the connection's
terminal state observable. The guard is local to one call and adds no caller-visible lease type.

The operation runs through `Effect.useReleaseNonParking` or the equivalent delivered primitive. If
structured cancellation or interruption reaches an armed operation, release first changes the
owner phase to `Closed` and invalidates pending TLS transfer cursors so no unknown prefix is
acknowledged or reoffered. It then consumes close authority for exactly one terminal provider close
attempt and preserves the cancellation outcome. Because the caller still owns the same value after
the interrupted borrow ends, every later I/O operation observes `Closed` and returns its existing
typed invalid-state failure before touching the provider.

This conservative rule applies even when cancellation arrives before a transfer is known to have
occurred; proving that an arbitrary provider performed no external work across suspension is not
available at this layer. Direct close itself already invalidates before its one nonparking provider
call and does not need to nest an operation guard. Synchronous metadata access performs no provider
work and also needs no guard.

Relying on an outer caller-installed bracket was rejected because direct ownership is public and a
caller can invoke I/O without a lease. Replacing all operations with a callback-only protected-use
API was rejected because it recreates the retention limitation this owner solves; the internal
guard gives direct owners the same mandatory cancellation safety without another public lifetime
surface.

### 6. Separate terminal close from graceful directional shutdown

Add `Closed` to the owned connection phase. Terminal `close(&mut self)` changes the phase to
`Closed` and invalidates/takes provider close authority before one `ByteDuplex.close`. It performs
no read, flush, readiness wait, `close_notify`, or retry. A close failure is observable to a direct
caller; bracket release recovers it so it cannot replace the protected success, failure, or
cancellation.

`shutdownWrite(deadline)` remains graceful and directional: it queues and drains `close_notify`,
flushes, invokes provider write shutdown, and leaves reads available. It may park and therefore is
not the finalizer.

Drop-only cleanup was rejected because generic provider policy belongs to the Effect boundary and
fatal traps intentionally promise neither finalizers nor Drop. Graceful finalization was rejected
because release must remain nonparking and independent of peer cooperation.

### 7. Expose the native owner that already exists internally

Factor the current private resolved-TCP and pathname-Unix acquisition helpers into public owned
constructors. They return the existing affine native `Connection` only in `Open`, with the same
options, absolute deadline, candidate order, typed errors, and descriptor invalidation rules.
Every provisional descriptor remains an ordinary owned local until successful return, so failure
or structured cancellation releases it exactly once.

Public scoped native helpers consume the result of these constructors and bracket it; they no
longer contain a separate acquisition path. Their callback service-row exclusion remains useful,
but callback scope is convenience rather than the only lifetime.

The previous native design rejected owned return to reduce caller cleanup obligations. That trade
cannot support an owned TLS connection. Protected constructors plus scoped convenience retain the
safety boundary without making the connection unretainnable.

### 8. Keep evidence at the cheapest discriminating boundaries

One structured analysis snapshot proves owner privacy, affinity, provider/provenance substitution
rejection, escaped-view rejection, service-row absence, explicit trust input, and removal of the
obsolete signatures. Existing TLS replay input proves authenticate/move/continue without another
full TLS conformance suite. The shared native corpus executes owned native acquisition, owned TLS
publication, success/failure/cancellation cleanup, and terminal close ordering. A representative
LLVM-to-Wasm memory-provider case proves the ordinary-source TLS owner and bracket path without
claiming native sockets on Wasm.

The TLS memory-provider witness additionally cancels a pending write through a directly retained
owner. Separate offer, close, and later-I/O counters prove that the operation-local guard marks the
same owner terminal, attempts one close, never reoffers the ambiguous ciphertext buffer, and rejects
subsequent I/O before provider dispatch.

JUL-23 later owns executable HTTP lifetime evidence. JUL-197 later owns pool interleavings and
capacity/eviction evidence. This change adds no pool or full HTTP test.

## Downstream Dependency Notes

JUL-23 must make its own normative decisions for pairing plain or owned TLS transport with owned
input/output buffers, constraining borrowed HTTP views, representing outer origin/route/security
context, and proving complete exchange disposition. This proposal intentionally defines no HTTP
owner, buffer wrapper, reusable token, or completion API.

Only after JUL-23 lands may JUL-197 specify reservation accounting, acquisition and active-return
guards, idle publication, trust-refresh handling, eviction, capacity, expiration, wait queues,
synchronization, and pool close. This proposal does not prescribe those state machines or require
old connections to migrate or close under a future context policy.

## Risks / Trade-offs

- **[A returned resource owner increases caller cleanup responsibility]** → Provide scoped
  convenience over the same owner, document explicit brackets, and install guards before either
  owned acquisition can suspend.
- **[Trust preparation can fail before transport ownership is transferred]** → Require callers to
  prepare the snapshot first and document that its errors do not trigger TLS provider cleanup.
- **[Trust copies can be large]** → Preserve explicit bounded `TrustSnapshot.copy` outside this API;
  downstream context-sharing policy remains JUL-23/JUL-197 work.
- **[Concurrent in-flight socket work may preserve the old rejection]** → Reconcile its proposal,
  design, spec, tasks, docs, and tests in the same implementation; do not retain dual acquisition
  paths.
- **[Moving transport state can lose authentication provenance]** → Keep provider, TLS state, and
  the limited TLS provenance in one affine struct and test move-through-guard behavior.
- **[A direct owner could otherwise outlive cancellation with ambiguous transfer]** → Arm every
  suspending operation internally, invalidate transfer state before one close attempt, and reject
  all later I/O before provider dispatch.
- **[Publishing after an elapsed external deadline permits late output]** → Check the selected
  absolute deadline before the first TLS byte, after suspension, and immediately before publication.
- **[Finalizer close can fail]** → Invalidate first, attempt once, retain the typed error for explicit
  close, and suppress only inside release so it never masks the protected outcome.

## Migration Plan

1. Reconcile the in-flight native-socket artifacts, expose owned resolved/Unix acquisition, and
   implement existing scoped helpers over those owners.
2. Refactor the TLS pump around private state/provider reborrows; add `OwnedConnection<P>`, guarded
   owned authentication, terminal close, and external-deadline clamping.
3. Remove trust loading from `tls_connection`: delete `ConnectionOptions.trust`, the `TrustSource`
   import/requirement, and `TrustSourceError` from every TLS connection signature and caller. Migrate
   each caller to prepare and move one independent snapshot before transferring its provider.
4. Replace every borrowed-provider `withClient` caller with owned construction or the consuming
   scoped convenience; delete the obsolete signature and fixtures.
5. Update native/memory evidence, ownership analysis, generated registration, examples, and public
   documentation together.

Rollback reverts this complete change and its caller migration. There is no compatibility shim,
persistent wire format, stored data migration, or partially supported dual API to retain.
