## Context

See [proposal.md](proposal.md) for motivation and
[the capability specification](specs/scoped-byte-duplex-tls-connection/spec.md) for the behavioral
contract. The work base already contains the transport-independent TLS 1.3 `Client`, bounded
certificate validation, replaceable trust snapshots, explicit clocks, randomness, allocation, and
the partial-byte TLS demand/progress protocol.

Two existing constraints determine the shape of this change:

- `Effect.ensuring` runs its finalizer for normal success and typed failure, but destruction of a
  parked `Execution` drops its frames without resuming that Effect. It therefore cannot promise
  transport release on structured cancellation.
- `ByteDuplex.close` must remain an Effect service operation so an abstract provider can close
  through the same typed boundary as its other operations. A pure `Drop` hook or `once fn` cannot
  obtain the borrowed service or execute that Effect.

JUL-188 is developed provisionally on JUL-187 PR #422 head
`9e2e9a893078743d90d5215828b3610763418f0c`. Its final branch must be rebased onto JUL-187's final
verified head, and the exact rebased diff must be re-reviewed and reverified.

## Goals / Non-Goals

**Goals:**

- Give providers and adapters one exact vocabulary for partial reliable-byte ownership, deadlines,
  directional shutdown, terminal close, and transport EOF.
- Keep TLS authentication, trust, time, entropy, allocation, and transport dependencies explicit
  and publish a connection only after the existing client proves authentication.
- Make structured cancellation close the borrowed duplex exactly once without widening fatal traps
  into recoverable Effect outcomes.
- Keep the compiler privilege minimal and target-neutral; all TLS and transport policy remains in
  ordinary Silk source.
- Make the memory provider deterministic enough to prove real parking, deadline selection, short
  transfer ownership, cancellation, and event order without network access.

**Non-Goals:**

- Physical sockets, DNS, filesystem trust discovery, OS trust-policy emulation, or a Stream API.
- Retrying an externally ambiguous transfer, silently weakening authentication, or treating
  transport EOF as TLS `close_notify`.
- Cleanup after fatal traps. The existing trap semantics continue to bypass finalizers and Drop
  hooks.
- General asynchronous finalization. The new cancellation-safe finalizer is deliberately restricted
  to nonparking Effects.

## Decisions

### 1. Model transport as one exclusive service with validated public wrappers

`silk.byte_duplex` defines `ReadTransfer`, `ByteIoOperation`, `ByteIoError`, and the exclusive
`ByteDuplex` service. The public operations are:

```silk
readSome(output: &mut [u8], deadline: Option<Instant>) -> ReadTransfer
writeSome(input: &[u8], deadline: Option<Instant>) -> usize
flush(deadline: Option<Instant>) -> ()
shutdownWrite(deadline: Option<Instant>) -> ()
close() -> ()
```

Deadline-bearing operations require the same exclusive `MonotonicClock` timeline. Empty reads and
writes complete before service dispatch. Nonempty successful transfers are checked at the module
boundary: `0 < count <= slice.length`. An impossible count returns `InvalidTransferCount`, moves
the lease into a terminal invalid state, and is never retried.

This keeps providers honest without changing the existing `Writer` all-or-error contract.
Returning `Option<usize>` was rejected because it conflates temporary unavailability, transport
EOF, and zero progress. Returning owned byte chunks was rejected because it obscures the caller's
buffer ownership and adds allocation to the transport boundary.

### 2. Use absolute deadlines and provider-owned readiness

Every deadline is an `Instant` from the active `MonotonicClock`. A provider checks `now` before
external work and after every readiness wait. When readiness and the deadline coincide, timeout
wins. Temporary unavailability suspends through cancellation-aware clock/scheduler machinery; it
never returns zero or spins.

The adapter computes one handshake deadline immediately before the first ClientHello transport
step and reuses it for every handshake read, write, and flush. It also checks the same deadline
between driver steps and immediately before publishing the connection. Recomputing a duration per
operation was rejected because fragmented I/O could otherwise renew the handshake indefinitely.

### 3. Keep the deterministic provider in its own actor module

`silk.memory_byte_duplex` defines `MemoryByteDuplex`, scripted read and write events, transport
phase, outbound bytes, cursors, and an observable operation log. Read events are scheduled as
`Data`, `End`, or a typed failure at an absolute instant. Write events accept a bounded positive
prefix or fail at an absolute instant. If an event is not ready, the provider waits until the
earlier of event readiness and the supplied deadline, then rechecks the clock.

Construction consumes owned scripts and preallocates bounded outbound/audit storage under an
explicit `Allocator`. The service methods themselves do not acquire an allocator: accepted writes
copy into that pre-sized storage, and capacity exhaustion is a typed provider failure. This keeps
the pinned `ByteDuplex` requirement rows exact instead of smuggling allocation into I/O calls.

The public script constructors admit only valid transfers. A narrowly exposed invalid-count test
action is retained solely to prove wrapper validation. Shared observation state is used when a
provider is moved into a `'static` scheduled body, so tests can inspect bytes and event order after
the owned provider is released.

A single module with special cases inside `byte_duplex` was rejected because the memory provider is
one concrete actor with state, scripts, and inspection operations of its own.

### 4. Let `Connection` own TLS state while the scope owns the duplex lease

`silk.tls_connection` defines a `Connection` actor containing the existing TLS `Client`, bounded
input scratch/state, and an `Open | PeerEnded | WriteShutdown | Invalid` phase. `withClient` borrows
the exclusive `ByteDuplex` for the complete scope and passes only `&mut Connection` to the callback;
the callback cannot independently access or retain the duplex.

`ConnectionOptions` carries `TrustLoadLimits` and `handshakeTimeoutNanoseconds`, with a constructor
that selects the 30-second default. `withClient` preserves callback error and service rows
generically while adding its explicit TLS, trust, I/O, clock, random, and allocator requirements.
TLS, trust-source, and byte-I/O errors remain distinguishable variants; allocation failure is not
wrapped.

The acquisition order is fixed: load one trust snapshot, sample wall clock once, create the client,
sample monotonic time, compute the one handshake deadline, drive the handshake, check the deadline,
then publish the authenticated callback. This prevents split validation instants or callback access
to a partially authenticated client.

Returning an owned connection was rejected because it would either let the borrowed service escape
or require an additional owned transport abstraction not present in this slice.

### 5. Drive the existing client with exact demand/progress ownership

Every adapter loop drains `Client.pendingOutput` before application output. It offers only the
currently pending ciphertext suffix to `ByteDuplex.writeSome`, validates the reported positive
count, and calls `Client.ackWritten(count)` only after that exact success. Failure or cancellation
never acknowledges or replays an unreported suffix.

Reads first drain mandatory TLS control output, then return already verified plaintext. When more
input is needed they read into bounded scratch storage and feed exactly `Data.count` bytes.
Underlying `End` is passed to `Client.endInput`, preserving the existing distinction between
handshake truncation, post-authentication truncation, and authenticated `close_notify`.

`flush` drains TLS output before the provider flush. `shutdownWrite` asks the client to create
`close_notify`, drains and flushes it, and only then invokes the provider's directional shutdown;
the read side remains usable. Scope finalization calls terminal `ByteDuplex.close` and makes no
graceful TLS promise.

An all-at-once write helper was rejected because it would erase the TLS client's explicit output
acknowledgment boundary. Treating transport EOF as clean TLS EOF was rejected because it would turn
truncation into apparent authentication success.

### 6. Add one cancellation-safe nonparking Effect finalizer primitive

Ordinary `silk.effect` exposes `Effect.ensuringNonParking(protected, finalizer)`. Its finalizer has
the form `once Effect<'env; () ! never ? S> + Intrinsic.NonParking`; the protected result remains
`A ! E ? R | S`. `tls_connection` recovers any typed `ByteDuplex.close` failure before supplying
the finalizer, so cleanup cannot replace the protected outcome.

The compiler recognizes only sealed `Intrinsic.finalizeEffectNonParking`. The intrinsic arms a
finalizer frame before protected execution. Normal completion runs it and disarms the frame only
after it finishes. Structured `Execution` cancellation invokes the same armed thunk through the
generated nonparking machine driver before dropping retained captures and provider bindings, then
disarms it. Exact-once state and nested scopes are LIFO. Success, typed failure, and cancellation
remain the original observable outcome. Fatal traps retain their current bypass semantics.

The finalizer's captures and provider arguments become explicit suspension/frame ownership. MIR
verification requires every cancellation-capable state inside the protected region to retain the
armed finalizer environment until it is run or disarmed. Native and LLVM-to-Wasm use the shared
lowering; no target-specific cleanup policy is introduced.

Strengthening existing `Effect.ensuring` was rejected because it currently permits parking
finalizers and has documented cancellation semantics. A pure `ensuringSync` was rejected because a
pure closure cannot call the Effectful `ByteDuplex` service. A Drop-owned transport lease was
rejected because it would move provider policy into a synchronous destructor and could not preserve
typed boundary behavior.

### 7. Prove behavior at the cheapest discriminating layer

Compiler evidence checks intrinsic registration/property rejection, finalizer MIR metadata,
provider retention, exact-once disarming, and LIFO cancellation. Existing execution-storage
acceptance infrastructure proves native and LLVM-generated WebAssembly cancellation behavior. The
shared native acceptance corpus proves ordinary source success, typed failure, dormant
cancellation, fragmented authenticated I/O, short output, shutdown order, and exact close count.

Focused standard-library analysis tests prove public shapes, invalid counts, deadline propagation,
error mapping, and connection state transitions without recompiling native binaries per claim.
Documentation examples are executable and provide memory transport, trust, time, randomness, and
allocation explicitly. CI selection is extended only where existing target coverage cannot express
the new capability.

Per-feature native-binary tests and timing assertions were rejected because they duplicate the
shared corpus and make the compiler suite slower without adding a distinct oracle.

## Risks / Trade-offs

- **[Cancellation finalization changes frame ownership and teardown]** → Arm before entering the
  protected region, verify retention in MIR, run through the generated nonparking driver before
  ordinary frame cleanup, and test nested LIFO/exact-once behavior on native and WebAssembly.
- **[A finalizer incorrectly marked nonparking could stall cancellation]** → Require the sealed
  `Intrinsic.NonParking` property at compile time and reject finalizers that can reach an external
  park. Keep the new combinator distinct from general `ensuring`.
- **[Provider cancellation can leave an external transfer ambiguous]** → Invalidate and close the
  connection; never acknowledge or retry the affected TLS buffer.
- **[Absolute deadlines are meaningless across clock providers]** → Require all deadline creation,
  readiness waits, and comparisons in one borrowed `MonotonicClock` scope; document that an
  `Instant` is provider-relative.
- **[The scripted provider could accidentally become a production transport abstraction]** → Keep
  it explicitly deterministic and memory-only, with no socket or host error translation policy.
- **[JUL-187 may change before it is verified]** → Keep JUL-188's initial base SHA recorded, do not
  modify JUL-187, and require a final rebase plus exact-parent review, local focused checks, and
  exact-head CI before handoff.

## Migration Plan

1. Add the intrinsic catalog, property checking, lowering, frame metadata, teardown behavior, and
   focused compiler evidence without changing existing `Effect.ensuring`.
2. Add and register `byte_duplex`, then the deterministic `memory_byte_duplex` provider.
3. Add `tls_connection` over the published JUL-187 client API and add focused/source acceptance
   evidence.
4. Generate standard-library embedding and reference documentation, update manifest/index/CI
   selection, and publish the coherent draft stacked on JUL-187.
5. When JUL-187 reaches its final verified head, rebase JUL-188 onto it, resolve only task-owned
   conflicts, regenerate artifacts, and rerun exact-diff reviews and required verification.

Rollback is removal of the single JUL-188 stack commit(s) and their generated registrations. No
persistent format or compatibility migration is introduced.
