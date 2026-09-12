## Context

See `proposal.md` and the two delta specs. The verified base already supplies affine bounded TLS record directions, explicit trust snapshots, deterministic certificate-path validation, strict certificate/SAN decoding, HTTPS reference matching, the selected AEAD, key-agreement and signature primitives, and TLS HKDF. The client must compose those actors in ordinary Silk without adding compiler privilege or acquiring a transport.

## Goals / Non-Goals

**Goals:**

- Make every transport, handshake, authentication, application, post-handshake, and close transition explicit and bounded.
- Use one owned certificate graph and one exact transcript for all authentication decisions.
- Preserve caller atomicity and stable output under arbitrary partial feed and acknowledgment.
- Reuse existing actors directly, including their semantic error families and ownership guarantees.

**Non-Goals:**

- TLS 1.2, PSK/resumption, 0-RTT, exporters, client credentials, QUIC, DTLS, server mode, ECH, certificate compression, URI parsing, IDNA conversion, Web PKI policy, revocation fetching, sockets, or deadlines.
- A boolean authentication token, verification bypass, ambient trust/time, insecure entropy fallback, key logging API, FIPS claim, side-channel certification, or physical zeroization guarantee.

## Decisions

### One outer owner with replaceable record epochs

`Client` owns its validated origin, ALPN bytes, trust snapshot, time, transcript contexts, handshake reassembly, peer certificates, authenticated evidence, application buffer, and current send/receive record directions. A private state enum controls legal operations. This keeps the record actor as the sole AEAD sequence owner and permits exact epoch replacement after key derivation or KeyUpdate. Splitting public handshake and traffic objects was rejected because it would expose a forgeable transition seam.

### Validate configuration before construction mutation

The constructor preflights all scalar limits, time, reference, and ALPN invariants before it consumes trust or allocates client buffers. It then copies the canonical DNS bytes and protocols and constructs initial plaintext record directions. IP origins are copied into the private origin union and never produce SNI. This makes every caller-controlled rejection atomic and avoids lifetime retention.

### Drive one complete semantic event at a time

Input enters the existing receiver until exactly one record is ready. The client either appends an admitted handshake fragment, handles one alert/CCS/application record, or reports backpressure. Handshake parsing reads the four-byte header only when present, rejects declared bodies before allocation, and stops after one complete message. Output is always one stable record owned by the sender; client-level acknowledgment performs state transitions only when the record becomes fully acknowledged. Demand precedence is fixed as output, plaintext, one-shot authenticated event, peer close, fully closed, then input.

### Keep parallel transcripts only until suite selection

The first ClientHello is streamed into SHA-256 and SHA-384. A new non-consuming `checkpoint` copies a SHA state and consumes only the copy. ServerHello or HRR selects the hash. HRR replaces the transcript with the synthetic message-hash before the retry message. Thereafter one private selected transcript receives exact handshake header and body bytes at their wire boundaries. Re-parsing or hashing record framing was rejected because it can diverge under fragmentation.

### Copy authenticated evidence, not a borrowed path

The client decodes a leaf-first owned certificate vector. Within one handshake step it borrows the leaf, remaining certificates, and snapshot anchors; validates the path; decodes the same leaf SAN; verifies the reference; and verifies CertificateVerify with that leaf key. It copies only stable indices, instant, revocation status, match index, and retained leaf DER into authenticated metadata. The borrowed `ValidatedPath` never enters the client object, which avoids self-reference.

### Separate mandatory control scheduling from application writes

The client holds one pending-control enum for client Finished, empty client Certificate, KeyUpdate, and close_notify. Control output always precedes application output. A KeyUpdate record is queued under the old sender and the sender is replaced only after its final byte is acknowledged; this preserves byte identity. Repeated peer or local update requests coalesce to one response/request, and crossing requests do not generate another response.

### Closure freezes each direction

Local and peer closure bits are independent. Receiving close_notify freezes input before later bytes are interpreted. A truncation marker waits behind already-authenticated buffered plaintext. Fatal peer/protocol/authentication errors store one terminal `TlsError`, invalidate both directions, and clear unreturned plaintext when required. The original error is returned synchronously; a fatal alert is queued only when no partially transmitted record makes clean framing impossible.

### Deterministic replay is the interoperability boundary

An opt-in rustls capture tool records fresh peer sessions because ring key exchange uses OS randomness. The default suite verifies immutable file hashes and replays committed bytes, transcript checkpoints and expected results without Rust or network. The chosen validation implementation uses one small core native program for a complete authenticated byte-driver path and four independently bounded programs for handshake policy, key updates, closure and post-handshake controls, and resource policy. Each uses one constructor call graph and remains no larger than the core program. This topology and its size bounds are evidence-engineering choices, not protocol or public-API contracts. One Wasm source covers only a representative deterministic memory path. Analysis tests prove affine borrows and provider rows without compiling extra binaries.

## Risks / Trade-offs

- **[The composed client is state-dense.]** → Keep parsing and scheduling helpers private, one-purpose, and table-driven; expose only the driver and authenticated getters.
- **[Handshake vectors can overgrow the default compiler suite.]** → Keep each focused native program below ordinary source-realization capacity, reuse lower-layer evidence, and keep one Wasm witness. Do not duplicate JUL-171 fragmentation matrices.
- **[Committed rustls sessions are not byte-regenerable.]** → Pin the rustls source and lockfile, record one capture, verify immutable checksums offline, and describe fresh generation as semantic-only.
- **[Logical secret invalidation is not physical erasure.]** → Keep secrets private and affine, release all storage on drop, and document the absence of guaranteed zeroization.
- **[Best-effort alerts can conflict with partially sent records.]** → Never splice. Surface the original failure immediately and emit an alert only when record framing remains valid.

## Migration Plan

This is a new green-field capability. Add the SHA checkpoints first, then the client actor, fixtures, shared registrations, and generated artifacts. Rollback deletes this change's module, fixture tree, registrations, docs, and SHA checkpoint additions; no persisted format or compatibility path exists.
