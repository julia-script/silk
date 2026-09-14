## Purpose

Define bounded incremental HTTP/1 exchanges whose transport, response views, deadlines, and reuse proof have one explicit owner.

## ADDED Requirements

### Requirement: Scoped ownership and precise transport failures

The HTTP session SHALL own its transport, buffers, origin/security context and completion disposition so a completed owner can be retained without callback borrows. Scoped entry points SHALL lend this owner. The client SHALL lend one connection and at most one exchange at a time, preserve generic callback channels, and prevent heads, bodies, trailers and tunnels escaping their owner. Explicit transport loans SHALL grant terminal logical close authority while physical ownership stays outside. Plain ByteIoError and secured ConnectionError SHALL remain distinct. Scope cleanup SHALL cover success, failure and structured cancellation without replacing the original exit or releasing physical resources twice.

#### Scenario: Abandon a response

- **WHEN** the callback returns with unread framed bytes or fails
- **THEN** the lease becomes terminal without hidden drain or reusable evidence

### Requirement: Staged incremental exchange

The client SHALL send validated heads, accept body prefixes, finish requests and trailers, iterate response heads, stream framed bodies, expose completed trailers, finish responses, and abort. Invalid stages SHALL fail before output. Body modes SHALL be Empty, KnownLength, and HTTP/1.1 Chunked; HTTP/1.0 unknown lengths SHALL fail preflight. Prefix progress and next-message suffixes SHALL be preserved. Final 3xx, 4xx and 5xx statuses SHALL remain response values. Reuse SHALL require flushed complete request, framed complete response, permitted persistence, and no anomaly or abandonment.

#### Scenario: Partial upload and next response

- **WHEN** transport accepts a prefix then fails, or response input contains a subsequent message suffix
- **THEN** the exact committed prefix remains observable and framing does not consume the suffix

### Requirement: Informational responses and continue

Informational heads SHALL have finite count and cumulative wire-byte limits, defaulting to 16 and 65536. Status 101 SHALL fail UnsupportedUpgrade. Require100 SHALL require HTTP/1.1 and a finite absolute continue deadline clamped to the overall deadline, emit and flush Expect, and permit upload only after 100. An early final SHALL skip upload and forbid reuse. Expiry SHALL return ContinueTimeout with no fallback or replay. Disabled SHALL finish upload before receiving and promise no concurrent early-response monitoring.

#### Scenario: Early rejection

- **WHEN** Require100 receives 103 then a final 417 before 100
- **THEN** both heads are bounded and no body producer runs or reuse proof is published

### Requirement: Exclusive tunnel handoff

Successful CONNECT SHALL be handed off only through an exclusive scoped tunnel operation retaining the buffered suffix and ignoring successful CONNECT framing headers. It SHALL consume HTTP reuse authority and SHALL NOT expose an ordinary response body.

#### Scenario: Tunnel suffix

- **WHEN** a successful CONNECT head shares input with tunnel bytes
- **THEN** the tunnel reads those bytes first and HTTP operations cannot resume

### Requirement: Origin and request preflight

Origins SHALL admit only http/https with checked nonempty host and port, reject userinfo, retain TLS reference identity independently of resolution/Host/proxies, use default ports 80/443, map empty path to slash, preserve query and omit fragment. Unix connections SHALL require an HTTP authority. Header controls SHALL distinguish Default/Omit/Value. Defaults SHALL generate Host, silk-http/1 User-Agent and */* Accept. HTTP/1.1 Host omission, conflicting authority/framing, duplicate authorization sources and direct Proxy-Authorization SHALL fail before wire output. Generated fields SHALL count against finite head and credential limits. Raw responses SHALL not negotiate decoding automatically.

#### Scenario: Invalid authority

- **WHEN** caller Host conflicts with origin or userinfo is supplied
- **THEN** request admission fails without HTTP output

### Requirement: Authentication and absolute deadlines

HTTPS SHALL authenticate before HTTP output with explicit trust, time, random and allocator services. HTTP/1.1 SHALL offer only http/1.1 ALPN, permit absent ALPN by default, reject absence under RequireAlpn and reject other selections. HTTP/1.0 SHALL disable ALPN. The unchanged overall absolute monotonic deadline SHALL cover phase boundaries; finite phase deadlines SHALL clamp against it. The delivered TLS external-deadline pump SHALL be reused. Native synchronous hostname resolution with an overall deadline SHALL return UnsupportedDeadline before dispatch; numeric lookup SHALL bypass hostname resolution. Unsupported native target profiles SHALL expose a documented unsupported result while deterministic source execution remains portable.

#### Scenario: Trust loading exhausts deadline

- **WHEN** trust acquisition crosses the external deadline
- **THEN** TLS transport output and HTTP output remain absent and the owner closes

### Requirement: Bounded delivery evidence

The implementation SHALL expose finite request/response byte, copied-header, credential, head/body/buffer/TLS limits and distinguish protocol, resolution, connection, trust, TLS, allocation and transport failures. Structured analysis SHALL prove loans and stages; shared deterministic native and intended Wasm acceptance SHALL prove runtime behavior. Public documentation and generated registrations SHALL describe the delivered surface without automatic retries, redirects, pooling or collection.

#### Scenario: Finite resource limit

- **WHEN** a configured request, response or informational budget is exhausted
- **THEN** a typed limit failure ends reuse without retry

### Requirement: Response-owned content decoding

The exchange SHALL bind content planning to its own current response head, request method, framing and encoded wire bytes. Decoding SHALL be selectable only once and before any raw body consumption or discard. The client SHALL reuse the delivered content core without erasing rich transport errors or adding automatic negotiation. Reuse SHALL require successful content and framing completion.

#### Scenario: One decoding authority
- **WHEN** a caller selects decoding for the current response
- **THEN** later decoding selection and raw body operations SHALL fail InvalidState before consuming more bytes, and content errors SHALL permanently prevent reuse.
