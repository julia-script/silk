# scoped-byte-duplex-tls-connection Specification

## Purpose

Define bounded partial-byte duplex I/O and one scoped adapter that publishes an authenticated TLS connection with explicit time, trust, ownership, and cleanup.

## Requirements

### Requirement: ByteDuplex exposes exact partial reliable-byte transfers

Canonical `silk.byte_duplex` SHALL expose the exclusive `ByteDuplex` actor with safe inherent `readSome(output: &mut [u8], deadline: Option<Instant>) -> ReadTransfer`, `writeSome(input: &[u8], deadline: Option<Instant>) -> usize`, `flush(deadline: Option<Instant>) -> ()`, `shutdownWrite(deadline: Option<Instant>) -> ()`, and `close() -> ()` operations over unsafe raw provider hooks. The raw close hook SHALL declare the sealed argument-free `Intrinsic.nonParking()` Effect-operation property; every selected implementation SHALL exclude external parking while nested transfer remains admissible. Each deadline-bearing operation SHALL require `&mut MonotonicClock`; each operation SHALL fail only with typed `ByteIoError`. `ReadTransfer` SHALL distinguish `Data { count }` from orderly underlying `End`. The safe wrappers SHALL validate every provider: a successful nonempty read or write SHALL report a positive count no greater than the supplied slice length, and an impossible count SHALL attempt terminal close before preserving `InvalidTransferCount` even when close fails. Empty calls SHALL make no provider I/O request. Existing `Writer` all-or-error behavior SHALL remain unchanged.

#### Scenario: Accept one short transfer

- **WHEN** a provider accepts or produces only a positive prefix of a nonempty slice
- **THEN** the operation reports that exact prefix and ownership of every unreported byte remains with the caller

#### Scenario: Reject an impossible transfer count

- **WHEN** a provider reports zero progress for a nonempty successful transfer or reports more bytes than supplied storage
- **THEN** the operation returns `InvalidTransferCount`, invalidates the lease, and never retries the ambiguous buffer

#### Scenario: Keep empty calls local

- **WHEN** a caller reads or writes an empty slice
- **THEN** the operation completes without invoking provider I/O and without reporting transport EOF

### Requirement: Deadlines and unavailability use one monotonic timeline

An absolute deadline SHALL belong to the active `MonotonicClock` provider's timeline. Before external work and after each suspended wait resumes, the provider SHALL compare `MonotonicClock.now()` with that deadline and return `ByteIoError.Timeout` when the deadline is reached. Temporary unavailability SHALL suspend with cancellation support and SHALL NOT return zero progress or spin. A provider that accepts a deadline SHALL integrate readiness with its deadline wait so an idle peer cannot prevent timeout. `None` SHALL mean no deadline.

#### Scenario: Timeout an idle peer

- **WHEN** no scripted readiness occurs before the supplied absolute deadline
- **THEN** the pending operation resumes on that deadline and returns `ByteIoError.Timeout`

#### Scenario: Prefer ready work before the deadline

- **WHEN** scripted readiness occurs before the supplied absolute deadline
- **THEN** the operation transfers the available positive prefix without waiting until the deadline

#### Scenario: Cancel a suspended transfer

- **WHEN** an owner cancels an Execution while a duplex operation waits for readiness
- **THEN** the wait releases its retained registration state exactly once and reports no successful transfer count

### Requirement: Duplex closure is directional before terminal close

`flush` SHALL ensure that accepted bytes reach the provider's documented transport boundary, not peer application receipt. `shutdownWrite` SHALL occur after preceding accepted output is flushed and SHALL close only the write direction. `close` SHALL be terminal and idempotent. A close failure SHALL remain typed when observed directly and SHALL NOT replace an earlier protected scope outcome during finalization. Orderly `ReadTransfer.End` SHALL describe sticky underlying transport EOF for the read direction, not TLS `close_notify`.

#### Scenario: Flush before directional shutdown

- **WHEN** a caller requests write shutdown after successful short writes
- **THEN** the provider observes every accepted prefix, then flush, then one directional shutdown in that order

#### Scenario: Close repeatedly

- **WHEN** terminal close is requested more than one time
- **THEN** the provider releases the underlying resource at most once and each later close remains terminal

### Requirement: withClient publishes only one authenticated scoped connection

Canonical `silk.tls_connection` SHALL expose a scoped `withClient` operation that takes and exclusively borrows one explicit concrete `ByteDuplex` provider, loads exactly one `TrustSource` snapshot, samples `SystemClock` exactly once, constructs the private TLS `Client` under explicit `Allocator` and `Random` providers, completes the handshake, and only then invokes one higher-ranked callback with an authenticated `Connection<'transport, P>`. Connection operations SHALL bind their private provider reborrow internally and omit `ByteDuplex` from their public service rows. The callback SHALL preserve its exact arbitrary service row `R` under the absence constraint `R in Without<R, &mut ByteDuplex>`, reject independent ambient access to the borrowed byte service, and prevent retaining the connection or provider reborrow after the scope. A failed lease SHALL never be reused.

#### Scenario: Delay callback publication

- **WHEN** path, identity, CertificateVerify, Finished, client-Finished acknowledgment, or handshake transport work is incomplete
- **THEN** `withClient` does not invoke the callback and releases the lease with the original typed failure

#### Scenario: Acquire ambient services once

- **WHEN** one scoped client handshake completes
- **THEN** it uses one owned trust snapshot and one sampled validation instant for the complete authentication decision

### Requirement: One absolute deadline bounds the complete handshake

After trust acquisition and TLS client construction, `withClient` SHALL sample `MonotonicClock.now()` immediately before the first ClientHello transport operation and compute one absolute deadline with `MonotonicClock.deadlineAfter`. The default handshake duration SHALL be 30,000,000,000 nanoseconds. The same `Some(deadline)` SHALL be passed through every handshake read, write, and flush. The adapter SHALL check the deadline between driver steps and immediately before it publishes authentication. `ByteIoError.Timeout` SHALL map to `HandshakeTimeout` and invalidate and close the lease. The deadline SHALL bound transport waits and cooperative driver progress, not prior trust loading, CPU preemption, or synchronous computation. The existing trap-on-unrepresentable-deadline contract SHALL remain unchanged.

#### Scenario: Use one handshake deadline

- **WHEN** fragmented input and short output require several driver iterations
- **THEN** every handshake transport call receives the same absolute deadline rather than a renewed duration

#### Scenario: Refuse authentication at the boundary

- **WHEN** the monotonic time reaches the deadline after TLS Finished but before authenticated publication
- **THEN** `withClient` returns `HandshakeTimeout`, invokes no callback, and closes the lease

### Requirement: Connection operations preserve TLS output ownership

Connection read, write, flush, and write-shutdown SHALL drive mandatory pending TLS control output before application output, pass application deadlines unchanged to the underlying duplex operations, and acknowledge only the exact count that `ByteDuplex.writeSome` reports. Pending ciphertext SHALL remain byte-identical across short writes. A provider error SHALL imply no success for an unreported suffix. Cancellation or suspension with unknown external transfer SHALL invalidate the connection, release the lease, and never retry that buffer on another connection.

#### Scenario: Send ciphertext without repeats

- **WHEN** one TLS record is accepted through several short provider writes
- **THEN** each byte is offered in order until its exact successful acknowledgment and no accepted byte is offered again

#### Scenario: Stop after an ambiguous cancellation

- **WHEN** a write is canceled after external transfer becomes unknowable
- **THEN** the adapter invalidates and closes the connection without replaying any part of that buffer

### Requirement: TLS closure and transport EOF remain distinct

After peer `close_notify`, connection reads SHALL drain already verified plaintext and then return clean EOF. Underlying `ByteDuplex.End` before peer `close_notify` SHALL map to TLS handshake truncation before authentication and TLS truncation afterward. Local write shutdown SHALL emit and flush `close_notify` before `ByteDuplex.shutdownWrite` and SHALL leave peer reads available. Finalization SHALL perform terminal `ByteDuplex.close` without promising a graceful network shutdown.

#### Scenario: Drain before clean EOF

- **WHEN** verified plaintext and peer `close_notify` arrive together
- **THEN** reads return all verified plaintext before they report clean EOF

#### Scenario: Detect premature transport EOF

- **WHEN** the underlying duplex returns `End` before peer `close_notify`
- **THEN** the adapter returns the corresponding sticky TLS truncation failure and closes the lease

#### Scenario: Half-close the write direction

- **WHEN** the caller requests write shutdown while the peer direction remains open
- **THEN** the adapter flushes TLS `close_notify`, shuts down only the underlying write direction, and can still receive peer data

### Requirement: Scoped finalization covers every structured Effect exit

The runtime SHALL expose only the smallest target-neutral sealed intrinsic needed for ordinary `silk.effect` source to attach one nonparking synchronous finalizer to a protected Effect. Ordinary source SHALL also expose a scoped resource/use/release bracket built on that primitive, lending one exclusively owned resource separately to higher-ranked use and release callbacks. The finalizer SHALL run exactly once after success or typed failure and during structured Execution cancellation or interruption, with the resource and required provider environment still valid. It SHALL preserve the original success, failure, or cancellation outcome. A recovered finalizer failure SHALL NOT replace that outcome. Fatal traps SHALL remain outside this guarantee and SHALL continue to bypass finalizers and Drop hooks.

#### Scenario: Preserve success after release

- **WHEN** protected work succeeds and its release finalizer completes
- **THEN** release runs once before the original success becomes observable

#### Scenario: Preserve typed failure after release

- **WHEN** protected work fails and its release finalizer completes or recovers its own close failure
- **THEN** release runs once and the original typed failure remains the observable outcome

#### Scenario: Finalize structured cancellation

- **WHEN** a parked protected Effect is canceled through its owned Execution
- **THEN** the nonparking finalizer runs once with its captured service environment before cancellation destroys the remaining frame values

#### Scenario: Keep fatal traps outside Effect cleanup

- **WHEN** protected work performs a fatal trap
- **THEN** no cleanup guarantee is made and the trap does not become a typed Effect outcome

### Requirement: The memory provider proves suspension and ownership deterministically

A canonical scripted memory `ByteDuplex` provider SHALL model fragmented reads, short writes, actual suspended reads and writes, EOF, typed provider error, deadlines, and cancellation at pending-output boundaries. Its readiness and timeout script SHALL use the supplied virtual monotonic clock. Evidence SHALL prove byte-exact output, transfer-count validation, flush and shutdown ordering, one 30-second handshake timeout, and scoped close on every structured exit while preserving the original outcome.

#### Scenario: Replay a fragmented authenticated session

- **WHEN** a deterministic script supplies a valid TLS peer flight in fragments and accepts output in short prefixes
- **THEN** `withClient` authenticates once, invokes the callback, and the provider records the exact expected ciphertext once

#### Scenario: Preserve a callback failure

- **WHEN** the authenticated callback returns its own typed failure and terminal close also fails
- **THEN** the scope reports the callback failure and records exactly one close attempt

### Requirement: Delivery remains portable, documented, and bounded

The new modules SHALL be registered in the standard-library manifest and generated embedding, documented through executable public API examples with explicit memory, trust, time, random, and allocator providers, and covered on native and LLVM-generated WebAssembly targets. Default evidence SHALL reuse the cheapest existing analysis and shared native corpus boundaries, SHALL include measured test-economics review, and SHALL require no network. The slice SHALL add no DNS, socket, filesystem trust discovery, physical transport provider, general Stream dependency, trust bypass, or ambient time or entropy.

#### Scenario: Verify the shipped surface

- **WHEN** manifest, generated source, generated reference, documentation examples, and target coverage checks run
- **THEN** both modules and every public member are present, documented, resolvable, and behaviorally covered without a physical socket
