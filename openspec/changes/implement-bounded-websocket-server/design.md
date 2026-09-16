## Context

See proposal.md for motivation and `specs/websocket-server-messages/spec.md` for the observable
contract. JUL-202 already validates the HTTP request, flushes 101, ends request-head borrows, and
lends the exact `BufferedDuplex<'transportView, P>` plus retained read-ahead to one higher-ranked
callback. `BufferedDuplex` owns fixed input/output state around an exclusive borrowed provider, but
currently has no direct terminal-close operation. The enclosing HTTP connection owns the physical
close authority and already releases it through the nonparking structured bracket.

The implementation is ordinary Silk source. Interface operations cannot introduce their own
non-lifetime type parameters, so the public session seam uses a higher-ranked once callback rather
than a method-generic handler interface. Existing buffered operations preserve exact partial-I/O
progress in `BufferError`; canonical `ByteDuplex.close` is terminal, idempotent, and nonparking.

## Goals / Non-Goals

**Goals:**

- Make one opaque affine session the only frame I/O authority inside an upgraded-channel scope.
- Validate all peer-controlled sizes and syntax before unsafe narrowing, allocation, payload read,
  or successful event publication.
- Preserve borrowed outgoing payload ownership and exact buffered progress across suspension and
  short writes.
- Give normal close, simultaneous close, protocol failure, truncation, limits, timeout, callback
  failure, and structured cancellation distinct finite outcomes.
- Reuse one existing scripted upgrade program for economical native and Wasm evidence.

**Non-Goals:**

- WebSocket continuation/fragment reassembly, extensions, compression, message streaming, a ping
  timer, ambient concurrency, or a `Stream` adapter.
- A listener, router, server framework, client implementation, TLS server, native `wss`, or any new
  network target.
- Compiler privilege, a second transport owner, hidden payload allocation, indefinite graceful
  cleanup, or broad compiler/pipeline verification.

## Decisions

### 1. Lend an opaque session through one higher-ranked callback

Add the public ordinary-source shape below (names are exact; formatting may be adjusted to the
canonical Silk formatter):

```silk
pub struct ServerWebSocket<'channel, 'transport, P> {
  channel: &'channel mut BufferedDuplex<'transport, P>
  limits: Limits
  stateValue: State
  failureValue: Option<Failure>
  header: [u8; 14]
  control: [u8; 125]
}

pub effect fn withServer<
  'callback,
  'channel,
  'transport: 'channel,
  A,
  E,
  ?R,
  P,
>(
  channel: &'channel mut BufferedDuplex<'transport, P>,
  limits: Limits,
  use: for<'call, 'channelView: 'call, 'transportView: 'channelView> once fn<'callback>(
    &'call mut ServerWebSocket<'channelView, 'transportView, P>,
  ) -> once Effect<'call; A ! E ? R>,
) -> A ! E | WebSocketError ? R | &mut MonotonicClock
where &'transport mut P provides &ByteDuplex from &mut ByteDuplex | &mut MonotonicClock,
  R in Without<R, ByteDuplex>
```

`withServer` validates public limit fields before creating a session and places the borrowed
session in `Effect.useReleaseNonParking`. Its release terminally closes the `BufferedDuplex`,
recovers that cleanup error, and therefore preserves the callback's exact success, failure, or
cancellation. The enclosing HTTP owner remains authoritative and its later idempotent close is a
no-op at the provider boundary. The callback cannot return the session, its internal channel, or a
borrowed event; excluding ambient `ByteDuplex` prevents bypassing serialized frame output.

Alternative considered: construct and return a movable socket owner. Rejected because JUL-202
lends, rather than transfers, `BufferedDuplex`; moving it would duplicate or steal HTTP close
authority. A provider-generic handler method was also rejected because current Silk interface
operations permit only method-scoped lifetime parameters.

### 2. Add one terminal BufferedDuplex seam, not another owner

Add `BufferedDuplex.close(self: &mut Self) -> () ! ByteIoError`. The operation marks the buffered
session terminal before calling canonical nonparking `ByteDuplex.close`, discards unread/pending
logical state without flush, and records that a close was attempted so repeated calls do not issue
another provider close even when the first returned an error. Existing enclosing scopes may still
invoke provider close; the canonical provider contract makes that terminal and idempotent.

This operation is used by clean peer close, fatal frame paths, and `withServer` release. It does not
add a new provider, deadline, allocation, or graceful-finalizer write. A graceful Close frame is
always an explicit operation performed before terminal close with an already supplied finite
deadline.

Alternative considered: wait for the outer HTTP callback to return. Rejected because `PeerClose`
must not be published as clean before the provider has been terminally closed, and a caller may
continue executing inside the upgraded callback after observing that event.

### 3. Use closed public values and a copyable sticky failure summary

Public values are:

```silk
pub struct Limits {
  pub maxMessageBytes: usize
  pub maxCloseFrames: usize
  pub maxCloseBytes: usize
}

pub enum LimitKind { MessageBytes, CloseFrames, CloseBytes, Arithmetic }
pub enum State { Open, CloseSent, Closed, Failed }
pub enum Operation { ReadEvent, WriteText, WriteBinary, WritePing, WritePong, SendClose, FinishClose }
pub enum ProtocolReason {
  MaskRequired, Rsv, ReservedOpcode, NonMinimalLength, LengthHighBit,
  ControlTooLarge, ControlFragmented, ClosePayloadLength, CloseCode,
}
pub enum Utf8Component { Text, CloseReason }
pub enum CloseDataReason { Code, ReasonTooLarge }
pub enum CloseLimitKind { Frames, Bytes }
pub enum Failure {
  Protocol, Fragmentation, Utf8, MessageLimit, BufferCapacity,
  CloseLimit, Truncated, Transport,
}

pub union WebSocketError {
  InvalidLimits { pub kind: LimitKind }
  InvalidState { pub operation: Operation, pub state: State }
  PreviousFailure { pub failure: Failure }
  BufferTooSmall { pub required: usize, pub capacity: usize }
  MessageTooLarge { pub length: u64, pub allowed: usize }
  UnsupportedFragmentation
  ProtocolError { pub reason: ProtocolReason }
  InvalidCloseData { pub reason: CloseDataReason }
  InvalidUtf8 { pub component: Utf8Component }
  CloseLimitExceeded { pub kind: CloseLimitKind, pub allowed: usize }
  Truncated
  Buffer { pub operation: Operation, pub frameAccepted: usize, pub error: BufferError }
  Transport { pub operation: Operation, pub error: ByteIoError }
}
```

The first failure returns its complete structured value, including `BufferError` and nested
`ByteIoError`. Because those external error actors are not copyable, `Failed` stores the copyable
`Failure` classification and later operations return `PreviousFailure` without I/O. This preserves
the root cause for state inspection without fabricating a second owned external error. `state()`
returns only the four selected states.

`Limits.defaults()` returns 65,536/32/65,536. `Limits.make` and `withServer` share pure admission so
public struct construction cannot bypass the positive-value checks. Checked input lengths remain
`u64` through canonicality and maximum checks; narrowing to `usize` happens only after it is proven
representable and within policy.

The public operation signatures are fixed as follows:

```silk
impl Limits {
  pub fn make(
    maxMessageBytes: usize,
    maxCloseFrames: usize,
    maxCloseBytes: usize,
  ) -> Result<Limits, WebSocketError>
}

pub fn state<'channel, 'transport, P>(socket: &ServerWebSocket<'channel, 'transport, P>) -> State

pub effect fn readEvent<'channel, 'transport, P>(
  socket: &mut ServerWebSocket<'channel, 'transport, P>,
  output: &mut [u8],
  deadline: Option<Instant>,
) -> Event ! WebSocketError ? &mut MonotonicClock

pub effect fn writeText<'message, 'channel, 'transport, P>(
  socket: &mut ServerWebSocket<'channel, 'transport, P>,
  message: string<'message>,
  deadline: Option<Instant>,
) -> () ! WebSocketError ? &mut MonotonicClock

pub effect fn writeBinary<'message, 'channel, 'transport, P>(
  socket: &mut ServerWebSocket<'channel, 'transport, P>,
  message: &'message [u8],
  deadline: Option<Instant>,
) -> () ! WebSocketError ? &mut MonotonicClock

pub effect fn writePing<'message, 'channel, 'transport, P>(
  socket: &mut ServerWebSocket<'channel, 'transport, P>,
  message: &'message [u8],
  deadline: Option<Instant>,
) -> () ! WebSocketError ? &mut MonotonicClock

pub effect fn writePong<'message, 'channel, 'transport, P>(
  socket: &mut ServerWebSocket<'channel, 'transport, P>,
  message: &'message [u8],
  deadline: Option<Instant>,
) -> () ! WebSocketError ? &mut MonotonicClock

pub effect fn sendClose<'close, 'channel, 'transport, P>(
  socket: &mut ServerWebSocket<'channel, 'transport, P>,
  data: Option<&'close CloseData>,
  deadline: Option<Instant>,
) -> () ! WebSocketError ? &mut MonotonicClock

pub effect fn finishClose<'channel, 'transport, P>(
  socket: &mut ServerWebSocket<'channel, 'transport, P>,
  deadline: Instant,
) -> () ! WebSocketError ? &mut MonotonicClock
```

Every operation has the corresponding `where &'transport mut P provides ...` constraint inherited
from `withServer`; it is omitted above only to keep the signatures readable. Free sibling
functions keep the actor-first style and avoid a large method surface.

### 4. Publish owned control metadata and caller-prefix data

Use opaque fixed-capacity values:

```silk
pub struct ControlPayload { bytes: [u8; 125], length: usize }
pub struct CloseReason { bytes: [u8; 123], length: usize }
pub struct CloseData { codeValue: u16, reasonValue: CloseReason }
pub union PeerCloseData { Absent, Present { pub code: u16, pub reason: CloseReason } }
pub union Event {
  Text { pub count: usize }
  Binary { pub count: usize }
  Ping { pub payload: ControlPayload }
  Pong { pub payload: ControlPayload }
  PeerClose { pub data: PeerCloseData }
}
```

`ControlPayload.asSlice`, `CloseReason.asSlice`, and `CloseData.code/reason` lend immutable views
from those owned values. `CloseData.make(code, reason)` validates code and UTF-8 and copies at most
123 bytes; it rejects 1010 because only clients use that extension-negotiation code. Absence is
represented by `Option<&CloseData>` at `sendClose`, never by synthesized 1005.

The exact pure constructor is
`CloseData.make(code: u16, reason: &[u8]) -> Result<CloseData, WebSocketError>`; invalid bytes return
`InvalidUtf8 { component: CloseReason }`, disallowed codes return
`InvalidCloseData { reason: Code }`, and oversized reasons return
`InvalidCloseData { reason: ReasonTooLarge }` before copying.

`Text` and `Binary` contain only a count. The caller keeps the output allocation and may inspect
its initialized prefix after the exclusive operation ends; the event cannot retain a borrow that
blocks the next socket call. Controls and peer close metadata copy from the one internal control
scratch before publication. This avoids a returned borrow into mutable session state.

### 5. Parse one complete unfragmented frame with fixed work

The private header reader uses `BufferedDuplex.fill/peek/consume` to accumulate only the two-byte
base header, the selected extended length, and four-byte mask. It validates in this order:

1. FIN/opcode and RSV profile;
2. required client mask;
3. control FIN and base-length constraints;
4. canonical extended-length form and 64-bit high bit;
5. policy maximum, checked `usize` narrowing, caller capacity, and close-attempt accounting;
6. payload input and in-place XOR using `(payloadOffset + index) % 4`.

Data reads go directly into caller output through exact bounded reads and unmask in place. Text
applies the same complete UTF-8 admission rules as `String.fromUtf8` incrementally while the
single affine output borrow is active, then publishes only after the full frame is present. On
failure, a partially initialized prefix is deliberately not returned as a message. Controls read
into the fixed 125-byte scratch.
One `readEvent` handles one frame only; Ping is published only after its automatic Pong flush and
Pong is never skipped.

WebSocket FIN-zero and continuation are the selected unsupported feature, not malformed syntax, so
they map to `UnsupportedFragmentation`/1003. Reserved opcode, RSV, missing mask, nonminimal length,
high-bit, oversized control, and invalid close shape/code map to `ProtocolError`/1002. Invalid text
or reason maps to 1007; policy message overflow maps to 1009. A legal caller-capacity miss is local
and performs no graceful Close.

Alternative considered: reuse one owned whole-message buffer. Rejected because it duplicates the
caller's storage, makes attacker-selected sizes an allocation concern, and weakens the exact
capacity failure boundary.

### 6. Serialize output with a header-first ownership cursor

`writeText`, `writeBinary`, `writePing`, `writePong`, and `sendClose` validate everything before
building a header. A private writer records aggregate frame bytes accepted, writes the unmasked
minimal header from the fixed 14-byte scratch, then borrows the application payload through every
`BufferedDuplex.writeAll`, and finally calls `flush` with a copy of the unchanged absolute
deadline. It does not coalesce or copy data payloads.

Once the first frame byte is accepted, any typed failure or cancellation makes the session
terminal; the finalizer closes it and no retry/fallback can corrupt the partial frame stream.
`Buffer { frameAccepted, error }` distinguishes bytes already accepted into the buffered actor
from the failing call's own acceptance/drain fields.

The state check precedes close-data validation for `sendClose`: `CloseSent` and `Closed` return
success without bytes regardless of the supplied value. In `Open`, a fully flushed Close changes
state to `CloseSent`; data and standalone control writes then reject. `finishClose` owns subsequent
protocol pumping.

### 7. Make peer and local close behavior one iterative state machine

Use one private frame step shared by `readEvent` and `finishClose`; do not recurse. `readEvent` in
`Open` publishes data/control events. A valid Peer Close is fully parsed before any reply. Open
replies once with identical data except 1010 becomes 1000/empty; CloseSent emits no second Close.
Only after reply flush and terminal close succeed does the actor enter `Closed` and publish
`PeerClose`.

`finishClose(deadline: Instant)` creates one `Some(deadline)` and passes copies unchanged to every
frame read, automatic Pong write, and flush. It checks the active monotonic clock before the first
step and between loop iterations. Counters include every inbound frame and all inbound header,
mask, and payload bytes. Before consuming a frame payload, the actor proves both counters remain
within the complete-call limits. It discards bounded data, answers Ping, ignores Pong, and stops
only on valid Peer Close or a typed failure. It never exposes discarded data.

`readEvent` is available only in `Open`. `finishClose` rejects `Open`, pumps only in `CloseSent`,
returns success without I/O in `Closed`, and returns `PreviousFailure` in `Failed`. Clean
byte-stream end before Peer Close is `Truncated`. After any terminal primary failure, the
actor stores its `Failure` summary, optionally attempts one mapped graceful Close only with an
unreached supplied deadline and unambiguous output, then terminally closes. Both cleanup errors are
recovered so the primary failure survives. Automatic response failure is itself the primary typed
transport failure and publishes no event.

### 8. Reuse the existing WebSocket acceptance topology

Refactor `websocketUpgradeAcceptance.ts` into shared upgrade support plus a frame/session fragment,
then extend the same source's main to perform frame cases after a real successful `withUpgrade`.
Keep the existing single native corpus entry and its single portable Wasm source/leg; rename the
case only if needed to describe the combined upgrade/server behavior. Do not create another test
file, worker, Analysis snapshot, runtime program, physical socket, TLS vector, backend matrix, or
fresh-process/stress/timing check.

Use one compact vector table for distinct header/close/UTF-8 boundaries. Commit the RFC section 5.7
bytes and one small deterministic independent generator script or documented derivation for the
125/126/65,535/65,536 boundaries; generated data should be represented compactly rather than as
large literals. Structured analysis covers lifetime escape/duplication and exact effect rows.
Static evaluation is limited to public pure limits and close-data admission; effectful frame I/O
is proven only by the shared runtime program. The user's narrow verification directive means
implementation runs only those feature-specific tests, plus formatting/diff checks; no pipeline,
full compiler suite, CI wait, or documentation generation task is added.

## Risks / Trade-offs

- [Unfragmented WebSocket support is intentionally incomplete] → Name 1003 as the selected response,
  document the subset prominently, and never claim full RFC 6455 interoperability.
- [A large legal data frame can exceed one caller buffer] → Return terminal `BufferTooSmall` before
  payload input; streaming and multi-call message assembly remain explicitly out of scope.
- [Buffered output may already own a frame prefix when failure occurs] → Track aggregate accepted
  bytes, close terminally, and never retry or emit fallback protocol bytes.
- [Two scopes can request terminal close] → `BufferedDuplex.close` records its attempt and the
  underlying canonical provider is idempotent, so one physical release occurs.
- [Close pumping can otherwise become attacker-controlled work] → Count frames and total wire bytes,
  enforce one absolute deadline, and check limits before payload reads.
- [Fixed control values enlarge returned events] → Accept the bounded copy cost to avoid mutable
  internal borrows or heap allocation.

## Migration Plan

This is a green-field actor. Add the buffered terminal-close seam, implement and register
`silk.websocket_server`, then migrate only the existing scripted upgrade fixture to exercise the
new nested session. Rollback removes that actor, manifest entry, fixture fragment, and close seam;
no persisted format or compatibility alias exists.
