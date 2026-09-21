## Purpose

Provide a bounded WebSocket server handshake that validates peer input and application acceptance before transferring exclusive buffered transport access.

## ADDED Requirements

### Requirement: Strict borrowed inspection

Inspection SHALL be pure, borrow the request head, require HTTP/1.1 GET and exactly one valid Host, key, and version field. Connection and Upgrade SHALL be valid comma-token lists containing upgrade and websocket respectively, case insensitive; Connection close SHALL reject. The key SHALL be canonical padded standard Base64 decoding exactly 16 bytes with only outer HTTP OWS trimmed. Transfer-Encoding, nonzero Content-Length, and Expect SHALL reject. Duplicate singleton fields SHALL reject even when equal. A syntactically valid single unsupported version SHALL map to 426 with supported version 13; malformed input SHALL map to 400 and Expect to 417.

#### Scenario: Canonical sample

- **WHEN** an otherwise valid request contains key `dGhlIHNhbXBsZSBub25jZQ==`
- **THEN** acceptance produces `s3pPLMBiTxaQ9kYGzzhZRbK+xOo=` from the original trimmed encoding and protocol GUID

#### Scenario: Duplicate or noncanonical key

- **WHEN** a request repeats its key or uses nonzero Base64 pad bits
- **THEN** inspection fails without allocation or protocol output

### Requirement: Explicit bounded negotiation

The application SHALL explicitly decide acceptance for this request before 101 output. Offers SHALL expose target, Host, optional single HTTP(S) serialized Origin or null, ordered unique subprotocol tokens, and extension presence. Origin SHALL NOT authenticate a client. Accepted subprotocol SHALL be absent or one exact offered token. Valid unsupported extensions SHALL be declined; malformed extension grammar and attempted extension selection SHALL fail. Additional validated HTTP headers SHALL forbid Connection, Upgrade, all WebSocket handshake fields, Content-Length, Transfer-Encoding, and Trailer, case insensitively, while preserving repeated Set-Cookie.

Defaults SHALL be 32 protocols, 128 bytes per protocol, 4096 aggregate extension bytes, 8192 serialized response bytes, and 16384 owned bytes. Checked finite overrides including zero SHALL be honored before output and before exceeding allocated capacity; index storage SHALL count toward owned storage.

#### Scenario: Browser extension offer

- **WHEN** an accepted request offers valid permessage-deflate syntax and the application selects no extension
- **THEN** the response omits Sec-WebSocket-Extensions

#### Scenario: Invalid decision

- **WHEN** the application selects an unoffered protocol or forbidden response header
- **THEN** the operation reports a typed decision error before any 101 bytes and HTTP rejection remains possible

### Requirement: Scoped exact handoff

The handshake SHALL bind inspection and decision to the active HTTP exchange, privately own its response plan, and end request-head borrows before handoff. It SHALL fully write and flush 101 before lending the same exclusively scoped buffered channel and exact unread suffix. HTTP operations SHALL be unavailable after success. Existing close authority SHALL stay armed throughout; partial-output failure, timeout, or cancellation SHALL close without fallback response or retry and preserve the protected outcome. Generic application failure and requirement channels SHALL be preserved. The same optional absolute deadline SHALL govern request acquisition, writes, and flushes.

#### Scenario: Coalesced first frame

- **WHEN** one transport read contains the request and first frame bytes and the response needs short writes
- **THEN** the upgraded callback starts after complete flush and receives the exact suffix once

#### Scenario: Flush failure

- **WHEN** the transport fails after any switching-response output
- **THEN** the upgraded callback does not run, the connection closes once, and no HTTP fallback is emitted

### Requirement: Portable ordinary-source implementation

Validation SHALL reuse the shared strict Base64 and SHA-1 actors without compiler privilege. Compile-time claims SHALL use Evaluation; runtime claims SHALL use shared native and intended LLVM-to-Wasm execution. Physical ws targets SHALL inherit the HTTP server listener targets. The API SHALL permit an explicitly supplied secured duplex without claiming native wss or TLS server support. Frames and compression are outside this capability.

#### Scenario: Offline validation

- **WHEN** the RFC example is evaluated without a network provider
- **THEN** validation and hashing need no OS or ambient transport service
