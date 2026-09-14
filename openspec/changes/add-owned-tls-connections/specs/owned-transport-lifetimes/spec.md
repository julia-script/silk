## Purpose

Define affine ownership and protected transfer of native and authenticated TLS transport state so
downstream clients can retain a complete connection without escaped loans, provider substitution,
or double close.

## ADDED Requirements

### Requirement: An owned TLS connection keeps protocol state and transport inseparable

Canonical `silk.tls_connection` SHALL expose an affine `OwnedConnection<P>` that owns the private
authenticated TLS state and the exact concrete `ByteDuplex` provider `P` used for its handshake.
Neither component SHALL be independently replaceable, extractable, copied, or paired with another
provider. Its operations SHALL borrow the owner exclusively, bind only its stored provider, omit an
ambient `ByteDuplex` requirement, and preserve `ConnectionError` and allocation failures without
erasing their nested causes.

The owner's private TLS client SHALL retain only TLS-observable facts established by its own
construction: the original reference identity supplied to the client, the fixed offered and
negotiated ALPN evidence, authentication evidence, and its one validation instant. A separate
private zero-state provenance marker SHALL be constructible only by this module after the final
authentication and deadline checks, recording the unforgeable association between that retained
client and the provider that carried its handshake without duplicating the client's evidence.
Borrowed public accessors MAY expose those facts read-only, but callers SHALL NOT forge or replace
them or the marker. Origin admission, resolved route, proxy, outer security-context identity, and
pool compatibility are outside this owner and this change.

#### Scenario: Move one complete authenticated owner

- **WHEN** authentication succeeds and the connection is moved into another ordinary-source owner
- **THEN** its TLS traffic state, pending bytes, TLS provenance, and original provider move together
  as one affine value

#### Scenario: Reject provider or provenance substitution

- **WHEN** source attempts to copy an owned connection, replace its provider, forge its TLS
  provenance, retain a borrowed connection view, or invoke its operations through an independent
  ambient `ByteDuplex`
- **THEN** analysis rejects the program before execution

### Requirement: Owned authentication begins with independently prepared trust

`authenticateOwned(provider, config, trust, options)` SHALL consume one provider and one already
independently owned `TrustSnapshot`. The caller SHALL load, decode, combine, or copy that snapshot
before invoking authentication; those preparation operations SHALL retain their own
`TrustSourceError | OutOfMemoryError` results and ownership rules. `ConnectionOptions` SHALL NOT
contain trust limits, and owned or scoped TLS authentication SHALL NOT import or require
`TrustSource` or include `TrustSourceError` in its failure row.

Trust preparation SHALL release any partial new owner on failure. Loading SHALL leave its source
service with the caller, while `TrustSnapshot.copy` SHALL leave the original snapshot unchanged and
caller-owned. A successful preparation yields the one independent snapshot later consumed by TLS;
it does not transfer or guard the transport.

Only after both owned arguments have been received SHALL `authenticateOwned` place the provider in
its private guard. It SHALL then sample `SystemClock` exactly once, consume the snapshot into the
existing bounded TLS client, and publish `OwnedConnection<P>` only after path, original HTTPS
reference identity, CertificateVerify, Finished, client-Finished acknowledgment, and the final
deadline check succeed. Until publication, ordinary frame ownership SHALL release the snapshot and
TLS allocations, while the guard SHALL terminally close the provider exactly once after typed
failure or structured cancellation/interruption. Fatal traps SHALL remain outside this guarantee.

#### Scenario: Trust preparation fails before authentication

- **WHEN** loading or copying the independent snapshot fails before `authenticateOwned` is invoked
- **THEN** that preparation reports its exact trust or allocation error, the provider has not been
  transferred into the authentication guard, its source or original snapshot remains caller-owned,
  and this operation promises no provider cleanup

#### Scenario: Cancel authentication after ownership transfer

- **WHEN** an owned handshake is canceled after both inputs were received while waiting for partial
  transport input or output
- **THEN** the guard closes the same provider exactly once, ordinary ownership releases the
  unpublished snapshot and TLS state, and no authenticated connection is published

#### Scenario: Preserve an authentication failure

- **WHEN** authentication returns a typed TLS, transport, or allocation failure and terminal close
  also fails
- **THEN** cleanup runs once and the original failure remains the observable result

### Requirement: Native outbound acquisition can publish a protected owner

Canonical `silk.native_socket` SHALL expose owned resolved-TCP and pathname-Unix acquisition that
returns its existing affine `Connection` only after the descriptor reaches `Open`. Every
provisional descriptor SHALL remain guarded through setup, candidate iteration, readiness, and
publication. Invalid input, candidate exhaustion, deadline, typed failure, or structured
cancellation SHALL close every unpublished descriptor exactly once. Scoped connection convenience
SHALL be implemented by acquiring that same owner and releasing it through the nonparking bracket;
callback-only acquisition SHALL NOT remain as a separate implementation or the sole public path.

#### Scenario: Retain an opened native connection

- **WHEN** resolved or Unix acquisition succeeds
- **THEN** the caller receives the same affine `Connection` that owns the opened descriptor and may
  move it into a TLS owner or another protected resource scope

#### Scenario: Cancel a native acquisition

- **WHEN** acquisition is canceled before the owner is published
- **THEN** its current provisional descriptor and candidate-local resources are released exactly
  once and no callback or successful owner becomes observable

### Requirement: Terminal close is nonparking, idempotent, and distinct from graceful shutdown

Owned plain and TLS connections SHALL expose explicit terminal close suitable for scope release,
abandonment, and cancellation. Terminal close SHALL invalidate the owner before invoking the
underlying close, SHALL attempt physical release at most once even when close reports failure,
SHALL perform no read, flush, `close_notify`, readiness wait, or retry, and SHALL remain terminal on
all later operations. TLS write shutdown SHALL remain a separate deadline-bearing operation that
drains TLS `close_notify`, flushes, and closes only the transport write direction. Direct close
failure SHALL remain typed; cleanup SHALL recover it so it cannot replace an earlier protected
outcome.

#### Scenario: Abandon without graceful network work

- **WHEN** an authenticated owner is abandoned by a protected scope
- **THEN** terminal close releases its transport without sending `close_notify`, flushing pending
  data, waiting for the peer, or attempting the provider a second time

#### Scenario: Preserve readable peer direction after graceful shutdown

- **WHEN** an active TLS connection successfully performs write shutdown
- **THEN** it has emitted and flushed `close_notify`, its write direction rejects new plaintext,
  and its peer-read direction remains available until terminal close
