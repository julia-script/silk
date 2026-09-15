## Purpose

Provide bounded, scoped WebSocket server message I/O after a validated HTTP upgrade, including
strict unfragmented frame validation and a finite, typed close-handshake lifecycle.

## ADDED Requirements

### Requirement: Scoped server sessions have explicit finite policy

Canonical `silk.websocket_server` SHALL expose an affine `ServerWebSocket` only through a
higher-ranked `withServer` callback over the exact `BufferedDuplex` lent by a successful
`silk.websocket_upgrade.withUpgrade`. The server value, its channel borrow, and any callback view
MUST NOT escape or be duplicated. `withServer` SHALL preserve the callback's generic success,
failure, and requirement rows, SHALL add only `WebSocketError` and `&mut MonotonicClock`, and SHALL
terminally close the same underlying provider after callback success, typed failure, or structured
cancellation/interruption without replacing the protected outcome. Fatal traps remain outside the
existing runtime cleanup contract.

`Limits.make(maxMessageBytes, maxCloseFrames, maxCloseBytes)` SHALL reject each zero value before
publication. `Limits.defaults()` SHALL select 65,536 message bytes, 32 close-attempt frames, and
65,536 close-attempt wire bytes. The session SHALL preallocate no payload-sized storage: it MAY
retain only a 14-byte frame-header scratch, a four-byte mask, one 125-byte control scratch, and
fixed close metadata. The public state query SHALL return exactly `Open`, `CloseSent`, `Closed`, or
`Failed`.

#### Scenario: Upgrade suffix stays authoritative

- **WHEN** the successful HTTP upgrade and a first frame arrive in one transport read
- **THEN** `withServer` consumes that exact retained suffix before requesting more provider input and creates no second transport owner

#### Scenario: Reject a zero policy

- **WHEN** any message, close-frame, or close-byte limit is zero
- **THEN** construction returns `InvalidLimits` naming that limit before it borrows or performs I/O on the upgraded channel

#### Scenario: Release a canceled session

- **WHEN** the scoped callback is canceled while a frame operation is suspended
- **THEN** the underlying provider is terminally released once and the cancellation outcome is preserved

### Requirement: Inbound frame headers are canonical and bounded

`readEvent(output: &mut [u8], deadline: Option<Instant>)` SHALL incrementally decode exactly one
client frame. It SHALL require the mask bit, all RSV bits zero, and an opcode of Text (1), Binary
(2), Close (8), Ping (9), or Pong (10). Continuation opcode zero or any FIN-zero data frame SHALL return
`UnsupportedFragmentation`, select close code 1003, and enter `Failed`. Reserved opcodes, nonzero
RSV, missing masks, nonminimal 16-bit or 64-bit lengths, a set 64-bit high bit, fragmented controls,
or control payloads above 125 bytes SHALL return `ProtocolError` with an exact `ProtocolReason`,
select close code 1002, and enter `Failed`.

The actor SHALL check 64-bit length rules, checked `usize` narrowing, `maxMessageBytes`, caller
capacity, and aggregate arithmetic before allocation or payload input. A data length above
`maxMessageBytes` SHALL return `MessageTooLarge { length, allowed }`, select close code 1009, close
terminally, and read none of that payload. A legal data length above `output.length` SHALL return
`BufferTooSmall { required, capacity }`, close terminally without graceful protocol output or
payload drain, and SHALL NOT classify the peer as malformed. No read operation SHALL hide a
whole-message allocation or unbounded work.

#### Scenario: Decode canonical extended lengths

- **WHEN** masked final data frames carry canonical lengths at 125, 126, 65,535, and 65,536 byte boundaries
- **THEN** the decoder chooses the required length form, narrows only after validation, and consumes the payload through caller storage

#### Scenario: Reject an attacker-sized frame from its header

- **WHEN** a canonical header declares a data message above `maxMessageBytes`
- **THEN** the actor returns `MessageTooLarge`, attempts only the finite graceful failure allowed by the supplied deadline, and never reads or allocates the declared payload

#### Scenario: Distinguish caller capacity from peer syntax

- **WHEN** a legal bounded data frame exceeds this call's output slice
- **THEN** the actor returns terminal `BufferTooSmall` with the required size, reads no payload, and emits no protocol-error Close

### Requirement: Events publish only completely validated payloads

One successful `readEvent` SHALL return at most one `Event`: `Text { count }`, `Binary { count }`,
`Ping { payload }`, `Pong { payload }`, or `PeerClose { data }`. Text and Binary SHALL name only the
initialized prefix of the caller's output; they SHALL retain no borrow into the mutable session.
Text SHALL publish only after the complete unmasked payload passes the same complete UTF-8
admission rules as `String.fromUtf8`. Ping, Pong,
and Close SHALL publish copied fixed-capacity metadata independent of later session mutation.
Invalid text or close-reason UTF-8 SHALL return `InvalidUtf8` naming the component, select close
code 1007, and publish no event. A read failure MAY have modified an output prefix, but that prefix
SHALL be documented as unusable and no successful payload view SHALL be published.

Ping SHALL be returned only after a byte-identical Pong has been completely written and flushed.
Pong SHALL be returned without a response. The actor SHALL NOT skip Pongs or other controls in a
hidden loop; an application deliberately pumps any desired finite number of events. Automatic
Pong failure SHALL preserve the exact `BufferError`/nested `ByteIoError`, publish no Ping event,
enter `Failed`, and retry no frame bytes.

#### Scenario: Read the RFC Hello frame across transport fragments

- **WHEN** `81 85 37 fa 21 3d 7f 9f 4d 51 58` arrives through arbitrary positive transport prefixes
- **THEN** one read returns `Text { count: 5 }` and the first five output bytes are `Hello`

#### Scenario: Surface Pong without an unbounded skip

- **WHEN** a valid Pong precedes a data frame
- **THEN** the first call returns the Pong metadata and only a later caller-selected read can return the data frame

#### Scenario: Fail before publishing an automatic response event

- **WHEN** the provider rejects any byte or flush of an automatic Pong
- **THEN** the read returns that typed transport failure, closes terminally, publishes no Ping, and emits no retry or fallback frame

### Requirement: Outgoing messages are validated before their first byte

`writeText(string, deadline)`, `writeBinary(input, deadline)`, `writePing(input, deadline)`, and
`writePong(input, deadline)` SHALL write one final unmasked server frame and flush it before
success. Text and Binary payloads MUST NOT exceed `maxMessageBytes`; Ping and Pong payloads MUST
NOT exceed 125 bytes. The actor SHALL validate state, opcode, text validity supplied by `string`,
length bounds, and checked header arithmetic before the first output byte. It SHALL encode the
minimal 7-, 16-, or 64-bit length form and SHALL never mask server output.

The caller's complete message SHALL remain borrowed through all short writes and the final flush.
Each accepted prefix SHALL advance exactly once; unaccepted bytes remain caller-owned. Any typed
failure or structured cancellation after output begins SHALL enter `Failed`, close terminally, and
SHALL NOT retry an uncertain suffix or write a fallback frame into the partial frame stream. No
unflushed or deferred-output public variant is provided.

#### Scenario: Write RFC Hello

- **WHEN** `writeText("Hello", deadline)` succeeds
- **THEN** the provider receives exactly `81 05 48 65 6c 6c 6f` in order followed by one flush

#### Scenario: Preserve short-write ownership

- **WHEN** a provider accepts one outgoing frame through several positive prefixes
- **THEN** each accepted byte is offered exactly once and success occurs only after the provider flush boundary

#### Scenario: Stop after partial output failure

- **WHEN** a write fails after accepting a frame prefix
- **THEN** the failure retains the exact buffered acceptance/drain progress, the session becomes `Failed`, and no suffix or fallback frame is attempted

### Requirement: Close data and peer close are validated exactly

An empty Close payload SHALL mean no status and no reason. One payload byte SHALL be a
`ProtocolError`. A payload of two or more bytes SHALL contain one big-endian code and a complete
UTF-8 reason of at most 123 bytes. Incoming codes SHALL allow assigned 1000 through 1003 and 1007
through 1014 plus application/private codes 3000 through 4999. It SHALL reject 1004, 1005, 1006,
1015, values below 1000 or above 4999, unassigned codes in the 1000 range, and 1016 through 2999.
The outgoing close-data constructor SHALL apply the same validation and additionally reject 1010;
disallowed codes and reasons above 123 bytes SHALL return exact `InvalidCloseData` reasons, while
invalid reason bytes SHALL return `InvalidUtf8`. An absent outgoing status SHALL encode an empty
payload and SHALL NOT synthesize 1005.

In `Open`, a fully valid Peer Close read by `readEvent` SHALL be answered once with identical absent status or
code/reason, except incoming 1010 SHALL be answered with code 1000 and an empty reason. The actor
SHALL flush that response, terminally close the provider, enter `Closed`, and only then publish
copied `PeerClose` metadata. Reply write, flush, or terminal-close failure SHALL publish no clean
PeerClose and preserve the exact typed transport error. `readEvent` SHALL reject `CloseSent`; only
`finishClose` consumes frames there. A valid Peer Close during `finishClose` SHALL close without a
second Close frame and complete successfully without exposing peer data or metadata. No Pong or
other response is emitted after peer Close.

#### Scenario: Accept an empty close reason

- **WHEN** a Close contains an allowed code and no reason bytes
- **THEN** the event preserves that code and an empty reason and the response repeats it exactly

#### Scenario: Normalize client-only 1010

- **WHEN** a valid client Close uses code 1010
- **THEN** the event reports 1010 but the server replies once with 1000 and an empty reason

#### Scenario: Complete simultaneous close

- **WHEN** a valid peer Close arrives after the server entered `CloseSent`
- **THEN** `finishClose` writes no second Close, closes the provider once, enters `Closed`, and completes without publishing an event

### Requirement: Local close completion is finite and idempotent

`sendClose(optionalCloseData, deadline: Option<Instant>)` SHALL, in `Open`, validate all supplied
data before output, write and flush one Close frame, and enter `CloseSent`, after which data writes
are unavailable. In `CloseSent` or `Closed`, repeated `sendClose` SHALL succeed without validating
its supplied data and without emitting bytes. In `Failed`, it SHALL return the sticky prior-failure
summary.

`finishClose(deadline: Instant)` SHALL require one caller-supplied absolute deadline, pass that
same value to every partial I/O, and check it between frames. It SHALL expose no data events. Across
the complete call it SHALL consume at most `maxCloseFrames` peer frames and `maxCloseBytes` inbound
wire bytes, including headers, masks, and payloads. Bound exhaustion or checked accounting
overflow SHALL return `CloseLimitExceeded` naming frames or bytes, close immediately, and perform
no further drain. Data received while `CloseSent` MAY be discarded only when its payload is at
most `maxMessageBytes` and the aggregate close bounds and deadline remain satisfied. Ping SHALL
still receive and flush an identical Pong; Pong SHALL be counted and ignored. A valid peer Close
completes as specified above.
`finishClose` SHALL reject `Open`, SHALL perform this loop only from `CloseSent`, SHALL succeed
locally without I/O from `Closed`, and SHALL return the sticky prior-failure summary from `Failed`.

#### Scenario: Repeat local close without validation

- **WHEN** `sendClose` is called after `CloseSent` or `Closed`, even with otherwise invalid supplied close data
- **THEN** it succeeds locally and writes no additional bytes

#### Scenario: Bound a peer that never closes

- **WHEN** `finishClose` reaches either configured aggregate bound before a valid peer Close
- **THEN** it returns `CloseLimitExceeded`, performs no further read, and terminally closes the provider

#### Scenario: Preserve one absolute close deadline

- **WHEN** control and discarded data frames require several reads, writes, and flushes
- **THEN** every operation receives the caller's unchanged absolute deadline and the actor checks it between frames rather than renewing a duration

### Requirement: Terminal outcomes and graceful failure remain distinct

Underlying orderly end before a valid Peer Close SHALL return `Truncated`, not `PeerClose`.
`BufferError` and its nested `ByteIoError`, including exact Timeout operation and partial progress,
SHALL remain typed. Malformed framing, unsupported fragmentation, UTF-8 rejection, message limits,
close limits, truncation, and transport failure SHALL enter sticky `Failed`; later operations SHALL
return `PreviousFailure` with the preserved copyable failure class and SHALL perform no I/O.
`Closed` SHALL remain distinct and terminal.

For `UnsupportedFragmentation`, `ProtocolError`, `InvalidUtf8`, and `MessageTooLarge`, the actor
SHALL attempt at most one corresponding Close (1003, 1002, 1007, or 1009) only when the originating
operation received an unreached `Some(deadline)` and output is still unambiguous. `None`, an
already-reached deadline, prior partial outgoing failure, truncation, caller `BufferTooSmall`, or
close-limit exhaustion SHALL skip graceful output and close terminally. A failure of the graceful
Close or final close MUST NOT replace the original error. No failure path reads more peer bytes.

#### Scenario: Distinguish EOF from protocol Close

- **WHEN** the provider returns orderly byte-stream end before a complete valid Close frame
- **THEN** the actor returns `Truncated`, enters `Failed`, and publishes no peer-close metadata

#### Scenario: Preserve a primary protocol error

- **WHEN** a malformed frame selects close 1002 and either that Close or terminal cleanup also fails
- **THEN** the observable failure remains the original `ProtocolError` and later calls report its sticky failure class without I/O

#### Scenario: Skip graceful failure without a deadline

- **WHEN** a fatal inbound validation error occurs during a read whose deadline is `None`
- **THEN** the actor writes no Close frame, closes terminally, and returns the original validation error

### Requirement: Delivery is portable, ordinary-source, and economically verified

Frame parsing, masking, length validation, UTF-8 checks, close-code policy, and server framing SHALL
be implemented in ordinary Silk source without compiler-known declarations. Pure validation SHALL
have feature-specific static/structured evidence. Runtime evidence SHALL extend one existing
scripted upgraded-channel program for shared native execution and exactly one intended
LLVM-to-Wasm leg; it SHALL use no physical socket, new worker, fresh-process check, stress matrix,
timing assertion, or per-vector compiler analysis.

Committed fixtures SHALL include the RFC 6455 section 5.7 masked Hello and ping/pong bytes, plus
independently generated boundary vectors for 125, 126, 65,535, and 65,536 and negative mask, RSV,
opcode, high-bit, nonminimal-length, control, UTF-8, and close-code cases. The repository SHALL
record the independent generator and purpose of each case. Tests SHALL distinguish WebSocket
fragmentation from ordinary fragmentation of transport reads and short writes. The public surface
SHALL document the selected unfragmented subset prominently and SHALL NOT claim complete RFC 6455
interoperability, native `wss`, TLS-server support, compression, or extension negotiation.

#### Scenario: Run one portable scripted session

- **WHEN** the shared corpus drives the same upgrade-plus-frame source on native and LLVM-to-Wasm
- **THEN** both targets preserve the upgrade suffix, validate and publish the same events, emit byte-identical frames, and release the provider exactly once

#### Scenario: Keep verification feature-specific

- **WHEN** implementation evidence is added
- **THEN** it reuses existing HTTP/WebSocket test workers and corpus programs and introduces no broad pipeline or generated-documentation gate
