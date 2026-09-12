# authenticated-tls13-client Specification

## Purpose

Define a bounded transport-independent TLS 1.3 client that authenticates one HTTPS server with explicit trust and time before it releases plaintext.

## Requirements

### Requirement: Construction owns validated configuration and authority

The standard library SHALL expose an affine `Client` whose exact constructor is `Client.make(config: &ClientConfig, trust: TrustSnapshot, validationTime: Instant) -> Result<Client, TlsError> ! OutOfMemoryError ? &mut Allocator | &mut Random`. `ClientConfig<'a>` SHALL contain `reference: ReferenceIdentity<'a>`, `alpn: AlpnConfig<'a>`, and `limits: ClientLimits`; `AlpnConfig<'a>` SHALL be `Disabled` or `Offered { protocols: &'a [AlpnProtocol<'a>], required: bool }`, with `Disabled` the documented default policy. Construction consumes one immutable trust snapshot, copies one validated HTTPS reference and ALPN policy, records one explicit canonical Unix-domain validation instant including nanoseconds, and allocates all reusable storage. It SHALL require explicit allocator and cryptographic-random services and SHALL read no ambient trust or time. DNS references SHALL be stored as lowercase ASCII A-label bytes and used unchanged for both certificate identity and SNI while preserving the accepted caller spelling only through that canonical value. IP references SHALL send no SNI. ALPN SHALL be disabled or an ordered, duplicate-free, nonempty list of at most 16 protocols, each 1..255 bytes and at most 1024 encoded bytes in total. Random-provider failure is fatal and SHALL NOT be translated, retried with another provider, or replaced by insecure entropy.

#### Scenario: Own one DNS identity

- **WHEN** construction receives a valid mixed-case DNS reference and explicit trust and time
- **THEN** the client owns one lowercase DNS value that controls both SNI and certificate identity after caller storage is released

#### Scenario: Reject invalid configuration atomically

- **WHEN** a limit, time, DNS reference, or ALPN policy is invalid
- **THEN** construction returns a typed error before publishing a client and does not retain or partly apply caller configuration

### Requirement: The public driver preserves partial-byte ownership

The client SHALL expose exactly `feedInput(&mut self, input: &[u8]) -> Result<Progress, TlsError> ! OutOfMemoryError ? &mut Allocator | &mut Random`, `progress(&mut self) -> Result<Progress, TlsError> ! OutOfMemoryError ? &mut Allocator | &mut Random`, `pendingOutput(&self) -> &[u8]`, `ackWritten(&mut self, count: usize) -> Result<Progress, TlsError>`, `readPlaintext(&mut self, output: &mut [u8]) -> Result<Progress, TlsError>`, `writePlaintext(&mut self, input: &[u8]) -> Result<Progress, TlsError>`, `requestKeyUpdate(&mut self) -> Result<Progress, TlsError>`, `closeWrite(&mut self) -> Result<Progress, TlsError>`, and `endInput(&mut self) -> Result<Progress, TlsError>`. `Progress` SHALL have public `consumed: usize`, `written: usize`, and `demand: Demand` fields. The two effectful handshake-driving operations allocate only for bounded peer certificates or generate a fresh HRR share; all other listed operations use preallocated storage and have no allocator, entropy, trust, or clock requirement. Input and plaintext borrows SHALL never be retained. Before `feedInput` or `progress` returns `NeedInput`, it SHALL process every complete buffered handshake message whose semantics are immediately available and schedule any resulting mandatory control output, including a requested KeyUpdate response. A zero-length plaintext destination SHALL return `EmptyBuffer`, never EOF, unless a sticky terminal error already exists; the original terminal error takes precedence over every caller-buffer check. Clean EOF is represented by zero `written` with `PeerClosed` or `Closed`; it never acknowledges output. One write SHALL accept at most 16384 bytes only after authentication, while the send direction is open, and when no mandatory control output is pending. Invalid caller input SHALL leave state unchanged. When output and another event are both ready, `NeedOutput` SHALL take precedence, then `PlaintextReady`, `Authenticated`, `PeerClosed`, `Closed`, and `NeedInput`.

#### Scenario: Acknowledge one stable output suffix

- **WHEN** transport acknowledges queued bytes in partial prefixes
- **THEN** every byte remains identical until acknowledged and accepted plaintext is never requested or encrypted again

#### Scenario: Refuse premature plaintext

- **WHEN** the peer path, identity, CertificateVerify, Finished, or local Finished acknowledgment is incomplete
- **THEN** plaintext read and write return typed invalid-state failure and no unauthenticated peer bytes are published

### Requirement: The handshake implements the selected TLS 1.3 profile

The client SHALL offer TLS_CHACHA20_POLY1305_SHA256, TLS_AES_128_GCM_SHA256, then TLS_AES_256_GCM_SHA384; X25519 then P-256; an initial fresh X25519 share; ECDSA-P256/SHA-256 and RSA-PSS-RSAE/SHA-256 handshake signatures; and RSA-PKCS1/SHA-256 additionally for certificate signatures. It SHALL offer only TLS 1.3, omit PSK, early data, compression, client credentials, post-handshake authentication, status_request, SCT, certificate compression, and middlebox session identifiers, and enforce the exact state path `ClientHelloPending -> AwaitServerHello -> AwaitEncryptedExtensions -> AwaitCertificate -> AwaitCertificateVerify -> AwaitServerFinished -> ClientFinishedPending -> Traffic`, with the single HRR detour through `RetryClientHelloPending`. A valid initial `CertificateRequest` is accepted only between EncryptedExtensions and Certificate, has an empty request context, contains one valid `signature_algorithms`, may contain valid `signature_algorithms_cert`, `certificate_authorities`, `oid_filters`, and empty `status_request` or `signed_certificate_timestamp` requests, and is declined with an empty client Certificate and no CertificateVerify. Framed unrecognized extensions SHALL be ignored. Every extension type, including an unrecognized type, SHALL occur at most once; a known extension not permitted in CertificateRequest, malformed optional extension, trailing bytes, or configured extension-budget excess is terminal. Certificate-list request context SHALL be empty. Every certificate entry SHALL contain a syntactically complete extension vector within the configured bound; because this client did not request any admitted per-entry extension, every such vector SHALL be empty. Duplicate extensions, unsolicited server extensions, forbidden later CertificateRequest, wrong order, trailing bytes, and illegal locations are terminal.

#### Scenario: Complete each selected suite

- **WHEN** a conforming peer selects any of the three offered suites and one admitted certificate signature scheme
- **THEN** the client derives the matching handshake and application epochs and completes authenticated traffic

#### Scenario: Decline an initial certificate request

- **WHEN** the server sends a valid initial CertificateRequest with an empty request context
- **THEN** the client sends an empty client Certificate followed directly by Finished

### Requirement: One bounded HelloRetryRequest replaces the transcript correctly

The client SHALL accept at most one valid HelloRetryRequest for P-256 or a bounded cookie. It SHALL reject a second retry, an already-offered-share request, an unsupported suite or group, illegal field changes, and a retry that changes nothing. It SHALL echo an admitted cookie verbatim, generate a fresh requested share, preserve other ClientHello fields except RFC-permitted changes, and replace the first ClientHello transcript with the selected hash's synthetic `message_hash`.

#### Scenario: Retry with P-256

- **WHEN** the server requests P-256 after the initial X25519 ClientHello
- **THEN** the client generates a fresh P-256 share and authenticates the second ClientHello with the exact synthetic transcript

#### Scenario: Reject a no-op retry

- **WHEN** a retry requests no new group and supplies no cookie change
- **THEN** the client returns sticky protocol failure and never publishes authentication

### Requirement: Authentication binds one leaf, path, identity, and transcript

The client SHALL decode a nonempty leaf-first certificate list with bounded entry extensions, validate the same owned leaf and remaining peer certificates against `TrustSnapshot.anchors` at the exact supplied canonical `SystemClock.Instant` for TLS-server use, decode and verify that same leaf's SAN against the owned HTTPS reference, and verify CertificateVerify with that leaf's public key over the exact TLS 1.3 server context and transcript. Years outside 1..9999 SHALL preserve the path validator's nested `InvalidTime`; the nanosecond field SHALL be retained in metadata and every validation decision SHALL use that single instant. It SHALL verify Finished with constant-work comparison. `Client.authentication(&self) -> Option<Authentication<'_>>` SHALL become `Some` only after the one-shot `Authenticated` transition; `Authentication` SHALL provide infallible `suite() -> CipherSuite`, `group() -> NamedGroup`, `selectedAlpn() -> Option<&[u8]>`, `leafDer() -> &[u8]`, `intermediateIndices() -> &[usize]`, `anchorIndex() -> usize`, `validationTime() -> Instant`, `revocationStatus() -> RevocationStatus`, and `sanIndex() -> usize`. These getters SHALL describe the exact same selected leaf/path/key graph and SHALL not accept or return a forgeable boolean proof token.

#### Scenario: Authenticate one coherent peer identity

- **WHEN** path, SAN, CertificateVerify, and Finished all succeed for one received certificate list
- **THEN** the client publishes authentication once and its getters describe that same leaf and selected path

#### Scenario: Preserve a nested validation failure

- **WHEN** certificate decode, path construction, or HTTPS identity verification fails
- **THEN** `TlsError` retains the complete typed dependency error in a distinguishable cause and authentication is never published

### Requirement: Transcript and epoch boundaries are exact

The client SHALL hash exact handshake headers and bodies, never record headers or reconstructed messages, into parallel SHA-256 and SHA-384 contexts until suite selection. A HelloRetryRequest or final ServerHello SHALL be the sole complete buffered handshake message at its plaintext record boundary before the client replaces the transcript or installs a protected receive epoch; coalesced plaintext handshake bytes at either transition are terminal. It SHALL install handshake and application keys at RFC-defined transcript boundaries, verify server Finished before installing server application-read keys, and publish authentication only after every byte of client Finished output is acknowledged. It SHALL accept and buffer at most one coalesced authenticated application record after server Finished while client Finished remains pending.

#### Scenario: Delay authentication until output acknowledgment

- **WHEN** server authentication succeeds but part of the client Finished record remains pending
- **THEN** the client retains authenticated state privately, may buffer one peer application record, and reports `Authenticated` only after full acknowledgment

#### Scenario: Match exact transcript checkpoints

- **WHEN** a committed replay reaches ClientHello, ServerHello, CertificateVerify, server Finished, and client Finished boundaries
- **THEN** its selected SHA checkpoint and derived traffic values match the immutable expected bytes

### Requirement: Compatibility and post-handshake controls are bounded

The client SHALL admit an exact `{1}` compatibility CCS only in the RFC handshake window and SHALL never hash it. After peer Finished it SHALL parse and discard at most eight NewSessionTicket messages and 262144 total encoded ticket bytes, derive no PSK, and process exact one-byte KeyUpdate values. Peer updates SHALL advance receive keys after old-key authentication. Requested send updates SHALL encode under old keys and activate new keys for the next record, preserve pending output, coalesce repeated requests, avoid crossing-update ping-pong, and occur automatically before the reserved final record slot.

#### Scenario: Cross key updates without ping-pong

- **WHEN** local and peer update requests cross while output is partial
- **THEN** each old-key control record remains stable, each direction advances once, and no redundant response loop starts

#### Scenario: Bound discarded tickets

- **WHEN** ticket count or encoded bytes exceed the configured inclusive budget
- **THEN** the client returns sticky `LimitExceeded` without retaining a resumption secret

### Requirement: Resource policy is explicit and inclusive

`ClientLimits.defaults()` SHALL return 262144 bytes per handshake body; 1048576 total handshake bytes including headers; 32 handshake messages; 16 peer certificates; 65536 DER bytes per certificate; 262144 total peer DER bytes; 4096 cookie bytes; 65535 extension bytes; one 16384-byte application plaintext buffer; 32 consecutive empty application or compatibility records; 64 consecutive post-handshake control messages; eight tickets; and 262144 encoded ticket bytes, plus the documented standard certificate-decode, SAN, identity, and path defaults. Configurable counts and byte limits SHALL be finite, positive, inclusive, no greater than their wire representation, and checked for arithmetic overflow before allocation. Certificate, identity, path, and trust limits SHALL remain separately named. A send epoch reserves the final slot and automatically queues KeyUpdate before record 8,388,608; a receive epoch accepts at most 8,388,608 records, refuses sequence wrap, and never retries an invalid tag with an alternate old or new key.

#### Scenario: Accept each exact limit

- **WHEN** every resource use equals its configured limit without arithmetic overflow
- **THEN** the client admits it and applies the corresponding protocol semantics

#### Scenario: Reject one-over resource use

- **WHEN** any message, total, count, cookie, extension, certificate, empty-record, control, or ticket budget exceeds its limit
- **THEN** the client returns sticky `LimitExceeded` with the exact resource category

### Requirement: Traffic closure and failure remain directional and sticky

Local close SHALL queue one close_notify after prior accepted output, reject later writes, and become final only after alert acknowledgment. Peer close_notify SHALL allow buffered authenticated plaintext to drain, then return clean EOF while local writes remain possible, and SHALL ignore all subsequent peer bytes without decryption. `user_canceled` SHALL be warning-equivalent, SHALL NOT close either direction, and a later EOF without close_notify SHALL still truncate. `Closed` SHALL require both directions closed and no pending output. Transport EOF without peer close_notify SHALL be `HandshakeTruncated` before authentication and `Truncated` afterward, after already-buffered verified plaintext drains. If EOF, pending output, buffered plaintext, authentication publication, or closure become simultaneously observable, the fixed demand precedence applies and EOF never consumes or acknowledges output. Fatal alerts, failed tags, and protocol or authentication failures SHALL invalidate both directions, cancel any pending sender suffix, discard unreturned plaintext for fatal authentication or protocol errors, and remain sticky. A transport that already wrote a prefix of canceled output must close. Best-effort fatal alerts SHALL never splice into a partial record or delay the original error.

#### Scenario: Ignore bytes after close notify

- **WHEN** a valid peer close_notify is followed by arbitrary transport bytes
- **THEN** the receive side stays cleanly closed and the trailing bytes cannot reopen it or create a new failure

#### Scenario: Drain before truncation

- **WHEN** authenticated plaintext is buffered before transport EOF without close_notify
- **THEN** reads return that verified plaintext before the sticky `Truncated` error

### Requirement: Failures remain precise and secrets remain private

The public `TlsError` union SHALL contain exactly `PeerAlert { code: u8 }`, `BadRecordMac`, `ProtocolViolation { reason: ProtocolReason }`, `CertificateDecode { error: DecodeError }`, `CertificatePath { error: ValidationError }`, `CertificateIdentity { error: CertificateIdentityFailure }`, `CertificateVerify { error: CertificateVerifyFailure }`, `Finished`, `UnsupportedProfile`, `LimitExceeded { kind: TlsLimitKind, limit: usize }`, `InvalidState { operation: ClientOperation }`, `EmptyBuffer`, `HandshakeTruncated`, `Truncated`, and `NoApplicationProtocol`. A fatal peer description maps to `PeerAlert` without collapsing its wire code; required-but-absent ALPN maps only to `NoApplicationProtocol`, while unsolicited, malformed, duplicate, or unoffered ALPN is a protocol/profile failure. Certificate path, identity, and signature errors SHALL preserve their complete nested taxonomy. Errors SHALL not expose traffic secrets, private shares, or transcript internals. Drop SHALL logically invalidate state and release owned storage after success, failure, defect, or interruption; physical secret zeroization and graceful destructor close SHALL not be promised.

#### Scenario: Keep the first fatal error

- **WHEN** any later operation follows a peer, authentication, or protocol failure
- **THEN** it returns the original typed terminal error without decrypting, emitting application data, or converting the result to EOF

### Requirement: Offline evidence covers interoperability economically

Repository evidence SHALL include immutable RFC 8448 component vectors and deterministic replay captures from rustls v0.23.35 commit `7768cd2b44049e040685d48318d13bfa7f7d32a8`, configured for TLS 1.3 with the ring provider and pinned by a checked-in Cargo lockfile and generator command/configuration. Captures SHALL record valid RSA-2048/SHA-256 and P-256/SHA-256 chains, exact DER and visibly test-only keys, entropy or ClientHello inputs, transcripts, intermediate digests, peer/version/command metadata, and checksums. Because rustls/ring obtains fresh ephemeral entropy from the OS, regeneration SHALL promise semantic outcome equivalence rather than byte-identical peer captures; the checked-in capture itself SHALL remain immutable and byte-verified offline. Coverage SHALL include three suites, both groups including forced P-256 HRR, both CertificateVerify schemes, one coalesced server Finished plus application-data delivery, and independently distinguishable negatives for DER, path, DNS/IP SAN, CertificateVerify, Finished, extensions, HRR, key shares, limits, ALPN, certificate request, tickets, CCS, KeyUpdate, closure, truncation, alert, and pending-output behavior. Default tests SHALL require no network or Rust toolchain. Native runtime evidence and a representative LLVM-to-Wasm memory-driver witness SHALL cover complete authenticated client use. This evidence SHALL reuse generic record-fragmentation and epoch-cap proof from the record actor. The comparison metadata SHALL preserve the fixed Zig HTTP parity snapshot `1bc892110da738d6137b3f0b7e8e3a586ce09928` without treating Zig as the conformance oracle.

#### Scenario: Verify fixture provenance offline

- **WHEN** the fixture-integrity check runs without network or Rust
- **THEN** all committed files, pins, commands, and hashes match the manifest

#### Scenario: Exercise the selected profile

- **WHEN** the native acceptance evidence runs
- **THEN** it distinguishes all selected suites, both groups, both CertificateVerify schemes, core authentication failures, bounded controls, and closure behavior
