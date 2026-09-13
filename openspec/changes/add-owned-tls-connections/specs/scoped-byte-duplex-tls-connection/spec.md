## MODIFIED Requirements

### Requirement: withClient publishes only one authenticated scoped connection

Canonical `silk.tls_connection` SHALL expose `authenticateOwned` as the authoritative constructor
for an affine `OwnedConnection<P>` that consumes and retains one explicit concrete `ByteDuplex`
provider. A scoped `withClient(provider, config, trust, options, callback)` convenience SHALL consume
the same provider and trust snapshot, invoke `authenticateOwned`, and lend only a higher-ranked
exclusive connection view before terminally closing the owner; it SHALL NOT contain a second TLS
pump or preserve the obsolete borrowed-provider constructor.

The caller SHALL prepare the independently owned `TrustSnapshot` before either operation receives
the provider. The operations SHALL remove `ConnectionOptions.trust`, SHALL neither import nor
require `TrustSource`, and SHALL exclude `TrustSourceError` from their failure rows. After receiving
the provider and snapshot, authentication SHALL consume that one snapshot and use one sampled
validation instant. Connection operations SHALL bind their owned provider internally and omit
`ByteDuplex` from their public service rows. Scoped callbacks SHALL preserve their exact arbitrary
service row `R` under the absence constraint `R in Without<R, ByteDuplex>`, reject independent
ambient access to the provider, and prevent retaining a connection view after the scope. A failed
owner SHALL remain terminal.

#### Scenario: Delay callback publication

- **WHEN** path, identity, CertificateVerify, Finished, client-Finished acknowledgment, deadline, or
  handshake transport work is incomplete
- **THEN** neither `authenticateOwned` nor `withClient` publishes an authenticated connection and
  protected cleanup preserves the original typed failure

#### Scenario: Scope the authoritative owner

- **WHEN** `withClient` authenticates and invokes a callback successfully
- **THEN** the callback operates on the same owned connection representation used by direct owned
  acquisition and terminal release closes it exactly once after the callback

#### Scenario: Acquire ambient services once

- **WHEN** one owned or scoped client handshake completes
- **THEN** it consumes one caller-prepared independent trust snapshot, samples one validation
  instant for the complete authentication decision, and performs no ambient trust-source load

### Requirement: One absolute deadline bounds the complete handshake

Owned authentication SHALL accept an optional external absolute monotonic deadline in addition to
the finite handshake duration. After receiving the prepared trust snapshot and constructing the TLS
client, it SHALL sample `MonotonicClock.now()` immediately before the first ClientHello transport operation,
compute the duration deadline, and select the earlier present deadline without converting the
external mark to a remaining duration. It SHALL reject an already reached selected deadline before
transport output. The default handshake duration SHALL be 30,000,000,000 nanoseconds. The same
`Some(deadline)` SHALL pass through every handshake read, write, and flush, with checks between
driver steps, after suspended waits, and immediately before authenticated-owner publication.
`ByteIoError.Timeout` SHALL map to `HandshakeTimeout` and invalidate and close the unpublished
owner. `None` for the external deadline SHALL leave the finite handshake duration in force. The
existing trap-on-unrepresentable-duration-deadline contract SHALL remain unchanged.

#### Scenario: Use one handshake deadline

- **WHEN** fragmented input and short output require several driver iterations
- **THEN** every handshake transport call receives one unchanged selected absolute deadline rather
  than a renewed duration

#### Scenario: Refuse authentication at the boundary

- **WHEN** monotonic time reaches the selected deadline after TLS Finished but before owned
  connection publication
- **THEN** authentication returns `HandshakeTimeout`, publishes no owner or callback, and closes the
  same transport exactly once

#### Scenario: Clamp to an earlier external deadline

- **WHEN** an overall request deadline precedes the internally computed handshake deadline
- **THEN** every partial handshake operation receives that unchanged earlier absolute mark and no
  later phase renews its duration

### Requirement: Connection operations preserve TLS output ownership

Owned connection read, write, flush, and write-shutdown SHALL drive mandatory pending TLS control
output before application output, pass application deadlines unchanged to the underlying duplex
operations, and acknowledge only the exact count that `ByteDuplex.writeSome` reports. Pending
ciphertext SHALL remain byte-identical across short writes. A provider error SHALL imply no success
for an unreported suffix. Cancellation or suspension with unknown external transfer SHALL
invalidate the complete owner; no part of that buffer SHALL be acknowledged, replayed on another
connection, or offered by a later operation.

Every suspending read, write, flush, and write-shutdown operation on a directly retained owner SHALL
install a private operation-local guard before it can call the provider. The guard SHALL disarm only
after exact returned progress has been committed or a returned typed failure has made the owner
terminal. If structured cancellation or interruption reaches an armed operation, its nonparking
release SHALL mark the owner `Closed`, invalidate all pending TLS transfer state before one provider
close attempt, and preserve the cancellation outcome. Later connection operations SHALL return the
existing typed invalid-state failure without invoking provider I/O. Fatal traps remain outside this
guarantee.

#### Scenario: Send ciphertext without repeats

- **WHEN** one TLS record is accepted through several short provider writes
- **THEN** each byte is offered in order until its exact successful acknowledgment and no accepted
  byte is offered again

#### Scenario: Stop after an ambiguous cancellation

- **WHEN** a caller retains a direct `OwnedConnection<P>` and cancels its write while the provider
  may have transferred an unreported prefix
- **THEN** the operation-local release marks that same owner `Closed`, invalidates the pending
  ciphertext before one provider close attempt, preserves cancellation, and never reoffers any part
  of that buffer
- **THEN** a later read, write, flush, or write-shutdown fails from terminal state without another
  provider operation

### Requirement: TLS closure and transport EOF remain distinct

After peer `close_notify`, owned connection reads SHALL drain already verified plaintext and then
return clean EOF. Underlying `ByteDuplex.End` before peer `close_notify` SHALL map to TLS handshake
truncation before authentication and TLS truncation afterward. Graceful local write shutdown SHALL
emit and flush `close_notify` before `ByteDuplex.shutdownWrite` and SHALL leave peer reads
available. Terminal owner close SHALL instead mark the owner closed before one nonparking
`ByteDuplex.close`, perform no graceful network work, and remain idempotent after success or close
failure. Direct close failure SHALL remain typed; protected finalization SHALL recover it without
replacing the earlier outcome.

#### Scenario: Drain before clean EOF

- **WHEN** verified plaintext and peer `close_notify` arrive together
- **THEN** reads return all verified plaintext before they report clean EOF

#### Scenario: Detect premature transport EOF

- **WHEN** the underlying duplex returns `End` before peer `close_notify`
- **THEN** the connection returns the corresponding sticky TLS truncation failure and remains
  unusable

#### Scenario: Distinguish graceful and terminal close

- **WHEN** one caller requests write shutdown and another abandons an owned connection
- **THEN** only write shutdown emits `close_notify` and preserves reads, while abandonment performs
  one immediate terminal transport close

#### Scenario: Half-close the write direction

- **WHEN** the caller requests write shutdown while the peer direction remains open
- **THEN** the adapter flushes TLS `close_notify`, shuts down only the underlying write direction,
  and can still receive peer data

### Requirement: The memory provider proves suspension and ownership deterministically

A canonical scripted memory `ByteDuplex` provider SHALL model fragmented reads, short writes,
actual suspended reads and writes, EOF, typed provider error, deadlines, cancellation at
pending-output boundaries, and observable ownership transfer. Its readiness and timeout script
SHALL use the supplied virtual monotonic clock. Evidence SHALL prove byte-exact output,
transfer-count validation, flush and shutdown ordering, external-deadline clamping, owned
publication only after authentication, movement of one complete provider/state pair, and exact
terminal close on every structured exit while preserving the original outcome. It SHALL also expose
distinct counters for pending direct-owned transfer, provider close, subsequent buffer offers, and
later I/O so cancellation can prove operation-local terminalization rather than only an outer scope.

#### Scenario: Replay a fragmented authenticated session

- **WHEN** a deterministic script supplies a valid TLS peer flight in fragments and accepts output
  in short prefixes
- **THEN** `authenticateOwned` returns one movable authenticated owner whose later operations
  continue the same byte-exact session without another handshake

#### Scenario: Preserve a callback failure

- **WHEN** the scoped convenience callback returns its own typed failure and terminal close also
  fails
- **THEN** the scope reports the callback failure and records exactly one close attempt
