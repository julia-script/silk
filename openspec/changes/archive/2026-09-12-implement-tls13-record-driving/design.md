## Context

See `proposal.md` and `specs/tls13-record-driving/spec.md`. The verified work base is `4ac4ffebd7c0258c4e93ebb5e084660278be88be`; its ordinary Silk standard library already provides detached AES-GCM and ChaCha20-Poly1305 plus TLS HKDF SHA-256/SHA-384. The active source tree places public modules under `packages/compiler/stdlib/silk` (the ticket's older `packages/stdlib/src/silk` path is historical). `RawBuffer` and `Allocator` provide exact owned storage, and lexical borrowing can make a returned record view structurally exclude concurrent mutation.

## Goals / Non-Goals

**Goals:**

- Keep every wire/state transition in ordinary source with allocation limited to constructors.
- Make progress and ownership explicit enough for one-byte and arbitrarily fragmented transports.
- Preserve authentication-before-publication and atomicity for all caller-controlled errors.
- Leave stable actor shapes on which JUL-187 and JUL-188 can build.

**Non-Goals:**

- Handshake messages, transcript state, certificate validation, server identity, alerts policy, automatic KeyUpdate, close_notify/truncation policy, sockets, readers/writers, and a connection adapter.
- OS trust equivalence, FIPS/constant-time certification, physical secret erasure, or a public sequence/test seam.

## Decisions

### One direction per affine owner

`TlsRecordSender` owns a single initialized 16645-byte `RawBuffer`, pending length/offset, private suite key/IV, and private sequence. `TlsRecordReceiver` owns a 16645-byte wire buffer and 16385-byte verified-plaintext buffer with framing, ready, terminal, and private epoch metadata. A `RecordView<'owner>` borrows the receiver's verified prefix. This is preferred to a bidirectional object because direction-specific ownership prevents accidental sequence sharing and lets later client state replace each epoch independently.

Each direction has `make(suite, trafficSecret) -> Result<Self, RecordError> ! OutOfMemoryError ? &mut Allocator` and `makePlaintext() -> Self ! OutOfMemoryError ? &mut Allocator`. Fixed key arrays and IVs live inline; only reusable variable-length buffers allocate. Secret width is validated and HKDF completes before buffer acquisition, so semantic rejection allocates nothing.

### Progress is data, misuse is an error

`feedInput` returns `InputProgress { consumed, demand }` with `NeedInput`, `RecordReady`, or `NeedRecordConsumption`. `queueRecord` returns `OutputProgress { accepted, demand }` with `RecordQueued` or `NeedOutput`; pending output is normal backpressure, not a failure. `ackWritten` returns a typed result because an excessive count violates the caller contract. `InvalidSecretLength`, `InvalidAcknowledgment`, `InvalidState`, `RecordOverflow`, `InvalidContent`, `AuthenticationFailed`, and `KeyUsageExhausted` remain semantic `RecordError` branches. This follows existing bounded decoder progress conventions while keeping peer failure distinct from transport suspension.

### Header-first admission and delayed publication

The receiver accumulates at most five bytes, validates outer type/version and the declared length, then copies only the admitted body's needed prefix. Protected records decrypt from the owned wire buffer into the separate decrypted buffer; AEAD failure leaves that destination untouched. Inner padding/type/content validation happens afterward, and `record` exposes a view only after all checks succeed. Every peer-originated rejection marks the receiver terminal. Plaintext records copy their admitted body into the same published buffer so record inspection has one ownership shape.

### Exact TLS 1.3 protection

Protected output is `application_data || 0x0303 || u16(ciphertext length)`, with the complete header as AAD. A temporary fixed 16385-byte local inner buffer holds `content || type` so detached AEAD inputs cannot alias the pending ciphertext. Input uses the owned wire buffer and a local tag copy so detached tag/ciphertext borrows do not overlap destination mutation. The nonce starts from the static IV and XORs the sequence into its final eight bytes in network order. Initial output has no padding; verified input strips any legal zero suffix and requires an admitted final type.

The record actor admits sequences 0 through 8388607. JUL-187 will reserve the final client send record for KeyUpdate before asking this actor to encrypt other established traffic; the standalone record actor has no handshake-policy knowledge and simply returns `KeyUsageExhausted` afterward.

### Canonical source injection for private boundary tests

The shared native acceptance source imports the public actor for normal behavior. A second compact fixture reads the canonical module source and appends same-module calls that construct private near-cap states, following existing cryptographic private-seam practice. This tests production helpers without millions of records or a public test-only setter. Structured analysis separately checks borrowed-view mutation and fixed-secret typing. One small Wasm witness covers only the intended portable record path.

## Risks / Trade-offs

- **[A second fixed sender allocation is required for detached AEAD input]** → The sender owns one 16,385-byte staging allocation beside its pending-wire allocation because the delivered AEAD actors reject overlapping plaintext and ciphertext buffers. Both are acquired at construction and reused without later allocation.
- **[Multiple constructor allocations can partially succeed]** → Normal affine drop releases earlier buffers if a later allocation fails; no state escapes until construction completes.
- **[Reference vectors can become expensive compiler fixtures]** → Keep one representative vector per suite/path and consolidate fragmentation/tamper cases in one shared native corpus program; use structural analysis for lifetime failures.
- **[Record protection may be mistaken for HTTPS]** → Module docs and actor docs explicitly disclaim peer identity and expose neither a client nor a verification bypass.

## Migration Plan

This is a new green-field actor. Add the source and manifest registration, generate documentation, and update shared acceptance registrations serially. Rollback is deletion of this ticket's module, manifest entry, fixtures, docs, and OpenSpec change; no stored data or compatibility path exists.
