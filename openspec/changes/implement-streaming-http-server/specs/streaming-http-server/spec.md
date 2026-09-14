## Purpose

Defines a bounded, scoped HTTP/1.0 and HTTP/1.1 server exchange that streams request and response
bodies while preserving message boundaries, connection ownership, and exact transport progress.

## ADDED Requirements

### Requirement: Server connections are scoped to one concrete buffered transport

The standard library SHALL provide a scoped server connection over an exclusively borrowed concrete
byte-duplex provider. Construction SHALL validate all finite capacities and limits before reading or
writing, then bind the provider and one input/output buffer pair for the connection lifetime.
Validation or allocation failure before that ownership transfer SHALL leave the caller-owned
provider untouched. Once bound, the connection SHALL preserve generic callback success, error, and
requirement channels and close the provider exactly once on success, typed failure, or structured
cancellation/interruption. Fatal traps SHALL remain outside the structured-cleanup guarantee, and
release SHALL perform no hidden flush or graceful drain.

#### Scenario: Connection and provider cannot escape or rebind

- **WHEN** a callback attempts to return the lent connection, a nested request/writer/channel loan,
  or to satisfy its operations with another ambient byte provider
- **THEN** ownership and requirement analysis rejects the program

#### Scenario: Structured exit closes without hidden I/O

- **WHEN** the callback succeeds, fails, or is structurally interrupted before explicit finish
- **THEN** the concrete provider is closed exactly once without an implicit flush, body drain, or
  blocking graceful exchange

### Requirement: One request head is admitted at a time with exact suffix preservation

The server SHALL parse request heads through the shared bounded incremental head parser and SHALL
not implement an alternate grammar. A request operation SHALL report clean end only when no head
byte has begun, report typed truncation for partial input end, lend head views from active request
storage, and preserve every byte after the terminating empty line. Connection states SHALL
distinguish Ready, RequestActive, Responding, Closing, Upgraded, Closed, and Failed; requests SHALL
be processed serially and a new request SHALL not start until the preceding disposition permits it.

#### Scenario: Coalesced pipeline remains serial

- **WHEN** one input fill contains a complete request and bytes of a second request
- **THEN** the first request head stops at its exact boundary, the suffix remains buffered, and the
  second request cannot be admitted until the first body and response disposition complete

#### Scenario: End between requests differs from truncation

- **WHEN** end is observed in Ready state with no new head byte
- **THEN** request admission reports clean connection end
- **AND WHEN** any start-line or field byte has already been accepted
- **THEN** it fails with typed head truncation and terminalizes the connection

### Requirement: Request bodies stream through shared framing with explicit disposition

Each active request SHALL select and decode its body using the shared HTTP body framing contract.
`readSome` SHALL retain no caller output borrow and SHALL report exact payload progress under one
unchanged optional absolute deadline. `discardRemaining` SHALL be explicit, finite, include chunk
syntax and trailers in its wire budget, and surface every framing, trailer, truncation, or discard
failure. The discard budget SHALL start at each call's decoder wire coordinate rather than include
wire accepted by earlier reads, and a typed decoder failure's committed consumed prefix SHALL be
removed from buffered input before the failure escapes. Request methods SHALL not imply an absent
body. The server SHALL perform no automatic request-body drain.

#### Scenario: Method-independent framed body is consumed

- **WHEN** GET or an extension method has valid fixed or chunked framing
- **THEN** body reads and discard honor that framing exactly rather than treating the method as
  body-free

#### Scenario: Reuse requires explicit body completion

- **WHEN** a handler begins a reusable final response with unread request content
- **THEN** the response is forced to close-after-response and no later pipelined request is parsed
- **AND WHEN** the handler explicitly discards within budget to framing completion before responding
- **THEN** request-body completion may contribute to reuse

### Requirement: Expect handling never waits before the application decision

HTTP/1.1 Expect parsing SHALL be ASCII case-insensitive. An equivalent list containing only
`100-continue` SHALL normalize to one pending continue action when the request has framed content.
The first body read or discard SHALL send and flush exactly one `100 Continue` before waiting for
input, even when body bytes are already buffered. Unsupported or parameterized expectations SHALL
select a final 417 response and close without waiting for body bytes. HTTP/1.0 SHALL ignore
`100-continue`; other unsupported expectations SHALL follow the explicit 417 policy.

#### Scenario: First body operation sends one continue

- **WHEN** an HTTP/1.1 request with actual framed content has mixed-case duplicate
  `100-continue` tokens and no final response has begun
- **THEN** the first body operation emits and flushes one 100 response before its first transport
  read and later body operations emit none

#### Scenario: Unsupported expectation is rejected immediately

- **WHEN** an unsupported expectation arrives with an unsent body
- **THEN** the server can emit and flush a final 417 and select close without reading that body

### Requirement: Informational responses are finite and precede one final response

An active request SHALL permit informational statuses 100 through 199 except 101, use the incoming
request version, emit no body or framing fields, and count both explicit and automatically generated
responses against one finite informational budget. The server SHALL never send 100 twice, send an
informational response after a final response begins, or begin more than one final response.

#### Scenario: Informational limit includes automatic continue

- **WHEN** automatic 100 consumes the final configured informational slot
- **THEN** a later explicit informational response fails with the precise limit before output

#### Scenario: Invalid informational head is atomic

- **WHEN** an informational head has status 101, a final status, forbidden framing, or incompatible
  version
- **THEN** validation fails before any response byte is accepted by the connection buffer

`Content-Length`, `Transfer-Encoding`, and `Trailer` SHALL all count as forbidden framing on an
informational response, Upgrade response, or successful CONNECT response.

### Requirement: Final heads and bodies are serialized once with explicit framing

A final response SHALL be validated and serialized through the shared HTTP head and outgoing body
framing contracts. The response writer SHALL support partial and complete payload writes plus
explicit trailer finish, retain no caller input borrow, and distinguish bytes accepted by bounded
server output from bytes accepted by the provider. A buffering/provider failure after encoder
progress SHALL carry the exact current writer-call payload prefix, buffered scratch prefix, and
cumulative encoder payload/wire totals. `NoBody` SHALL reject nonempty body writes;
fixed, chunked, and close-delimited modes SHALL enforce their normal completion rules. Chunked SHALL
require HTTP/1.1 and close-delimited SHALL be response-only. HEAD, 1xx, 204, 304, and successful
CONNECT response semantics SHALL come from the shared framing contract; a HEAD Content-Length
remains caller-supplied hypothetical representation metadata.

#### Scenario: Special response rejects body output

- **WHEN** a HEAD, 1xx, 204, 304, or successful CONNECT response selects no transmitted body and the
  handler attempts a nonempty write
- **THEN** the write fails with typed unexpected payload rather than silently discarding it

#### Scenario: Final response and finish are unique

- **WHEN** final head bytes may have escaped or one body finish has completed
- **THEN** another final response, fallback response, or finish attempt is rejected and cannot emit
  a duplicate prefix or terminator

### Requirement: Persistence is computed before final head output

HTTP/1.1 persistence SHALL be possible only when neither peer nor application selected close, the
request body is completely framed, the response is self-delimited, the request-count budget admits
another exchange, and no terminal error occurred. Across repeated Connection fields,
case-insensitive `close` SHALL override keep-alive. HTTP/1.0 persistence SHALL default off; explicit
opt-in SHALL additionally require an incoming keep-alive token, self-delimited request and response,
and an emitted keep-alive indication. Unknown-length HTTP/1.0 output SHALL be close-delimited and
never chunked. The server SHALL add or normalize its Connection control indication before any final
head byte is written and SHALL not parse another request after selecting close.

#### Scenario: Final admitted request advertises close

- **WHEN** the current request reaches `maxRequestsPerConnection`
- **THEN** the emitted final head selects close before output rather than discovering exhaustion
  after a keep-alive response

#### Scenario: Close token wins across fields

- **WHEN** repeated Connection fields contain both keep-alive and close in any casing or order
- **THEN** the response selects close and buffered pipeline bytes are not dispatched

#### Scenario: HTTP/1.0 keep-alive is explicit

- **WHEN** HTTP/1.0 persistence is disabled or the peer omitted keep-alive
- **THEN** the response closes even if both messages are otherwise self-delimited
- **AND WHEN** opt-in, peer token, message delimitation, and remaining budget all hold
- **THEN** the server emits keep-alive and may admit the next request

### Requirement: Every server resource is finite and checked

Limits SHALL include finite request-head and body/trailer limits, positive fixed read/write
capacities, `maxRequestsPerConnection`, `maxInformationalResponses`, `maxDiscardWireBytes`, and
`shutdownDrainBytes`. Zero SHALL remain a meaningful bound, including a zero request limit that
closes without reading a head. All allocation, count, and byte arithmetic SHALL be checked before
the governed resource is committed; there SHALL be no unlimited sentinel.

#### Scenario: Zero request budget performs no read

- **WHEN** a connection is created with `maxRequestsPerConnection` equal to zero
- **THEN** request admission selects close without reading any transport byte

#### Scenario: Exact fit and one over are distinct

- **WHEN** a head, body, discard, informational, request-count, or owned-storage resource exactly
  reaches its configured bound
- **THEN** the operation can complete
- **AND WHEN** the next governed unit would exceed that bound
- **THEN** a precise typed limit failure occurs before accepting it

### Requirement: Failures preserve recovery boundaries and committed progress

The server SHALL expose typed head, framing/trailer, byte-I/O, invalid-state, resource-limit,
unsupported-expectation, and caller failures. Before output, a locally invalid response SHALL leave
the active request available for another bounded response when framing state permits. Once response
bytes may have escaped, output failure or cancellation SHALL be terminal and SHALL not trigger a
fallback response or retry. An explicit caller failure mapper MAY produce a response only for an
untouched handler failure; default behavior SHALL abort without hiding the original typed failure.

#### Scenario: Pre-output validation remains recoverable

- **WHEN** final response validation fails before any byte is accepted into output state
- **THEN** the request remains active and the handler may choose another valid bounded response

#### Scenario: Partial output is terminal

- **WHEN** a short write accepts a response prefix and a later provider failure or cancellation
  occurs
- **THEN** the error reports committed progress, the connection becomes Failed, and no automatic
  fallback or replay occurs

### Requirement: Explicit connection finish performs bounded graceful shutdown

At a safe boundary, explicit finish SHALL flush the final response, shut down writes, drain at most
the caller-selected finite byte bound until end or the unchanged absolute deadline, and then close.
It SHALL not validate an abandoned request or dispatch pipelined data. Early Expect rejection SHALL
send and flush the final response before optional drain. Explicit finish failures SHALL be reported;
scope finalization SHALL preserve the protected callback outcome and use abortive nonparking close.

#### Scenario: Graceful order and drain bound are observable

- **WHEN** explicit finish is requested after a completed final response
- **THEN** flush precedes write shutdown, at most the configured drain bytes are read, and close is
  last even when end arrives earlier

#### Scenario: Cancellation does not promise graceful exchange

- **WHEN** the owning scope is canceled without successful explicit finish
- **THEN** it performs only nonparking close and does not hide cancellation behind flush or drain

### Requirement: Upgrade and tunnel handoff retain scoped buffered ownership

Upgrade and tunnel operations SHALL consume the active HTTP disposition, completely write and flush
the switching response, then lend the same buffered channel with its exact unread suffix to a
scoped callback. No HTTP operation SHALL be available afterward and transport ownership SHALL remain
inside the original scope. Generic Upgrade SHALL require a matching valid request Upgrade token,
Connection token membership, one offered selected protocol, a body-free request boundary, and a 101
response without forbidden framing. Tunnel SHALL require CONNECT, a valid 2xx response, and no
request body in this profile. Neither handoff SHALL return to HTTP persistence.

#### Scenario: Coalesced upgrade bytes transfer exactly once

- **WHEN** post-head protocol bytes arrived in the same input fill as a valid Upgrade request
- **THEN** the callback observes the exact unread suffix once and prior request/head/writer APIs are
  unavailable

#### Scenario: Failed switching output cannot fall back

- **WHEN** any switching-response byte may have escaped and later output or flush fails
- **THEN** the connection closes terminally without another HTTP response

#### Scenario: Body-bearing CONNECT is rejected before handoff

- **WHEN** a CONNECT request selects nonempty body framing
- **THEN** tunnel handoff fails before the 2xx response is emitted

### Requirement: Native admission serves exactly one accepted connection

The native adapter SHALL accept one connection per invocation through the existing scoped listener
API, provide that accepted connection as the concrete byte-duplex provider within the same owner
scope, and run the HTTP connection until close, explicit handler stop, or budget exhaustion. It
SHALL create no background accept queue, router, supervisor, detached owner, or unbounded task. Its
target availability and WebAssembly rejection SHALL exactly inherit the selected native listener
and connection contract.

#### Scenario: Serial native admission retains listener policy

- **WHEN** native admission is invoked once
- **THEN** exactly one accept occurs, the listener's OS backlog policy is unchanged, and another
  connection requires another explicit invocation

#### Scenario: Portable server excludes native admission

- **WHEN** a scripted byte transport targets intended LLVM-to-Wasm execution
- **THEN** the portable server actor remains usable while public native admission is rejected by
  target selection before backend or foreign-symbol discovery

### Requirement: Server deadlines remain absolute and operation-scoped

Every optional deadline SHALL be an absolute value on the active MonotonicClock timeline and SHALL
be passed unchanged across parser fills, body fragments, buffered short writes, flush, shutdown, and
drain. Deadlines SHALL not claim to preempt arbitrary handler CPU or synchronous provider work, and
graceful shutdown SHALL require an explicit finite deadline.

#### Scenario: Fragments do not refresh a deadline

- **WHEN** one request or response requires multiple transport operations
- **THEN** every operation receives the original absolute deadline rather than a renewed duration
