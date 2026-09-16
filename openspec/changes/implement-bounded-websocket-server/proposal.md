## Why

JUL-202 can validate and flush an HTTP WebSocket upgrade, but the upgraded channel still exposes
only bytes. Applications need one bounded server-frame actor that validates client masking,
lengths, text, control frames, and close semantics before publishing an event or accepting output.

## What Changes

- Add ordinary-source `silk.websocket_server` with a scoped affine `ServerWebSocket`, explicit
  finite limits, typed states and failures, bounded one-frame event reads, and flushed server frame
  writes.
- Implement the selected unfragmented RFC 6455 profile: masked client frames, unmasked server
  frames, canonical lengths, text and close UTF-8 validation, automatic Pong and close replies,
  and finite close completion.
- Reuse JUL-202's exact upgraded `BufferedDuplex` and unread suffix without another parser, buffer,
  transport owner, or hidden loop that skips peer events.
- Add a terminal buffered-channel close operation so fatal protocol paths and completed close
  handshakes can physically close before they publish their result; the enclosing scoped owner
  remains authoritative and repeated close stays idempotent.
- Add the smallest feature-specific pure, structured-analysis, and shared scripted native/Wasm
  evidence. Full pipeline runs, CI orchestration, and generated documentation are outside this
  change's implementation-task list.

## Capabilities

### New Capabilities

- `websocket-server-messages`: bounded unfragmented server frame/message validation, writing,
  close-handshake lifecycle, and scoped upgraded-channel ownership.

### Modified Capabilities

None.

## Impact

The change affects ordinary Silk standard-library sources for WebSocket frames and the minimal
buffered close seam, standard-library registration/catalog sources, and existing compiler
acceptance support/corpus files. It depends on `silk.websocket_upgrade`, `silk.buffered_duplex`,
`silk.byte_duplex`, `silk.string`, monotonic deadlines, and the existing nonparking scoped release
contract. It adds no compiler privilege, socket/listener path, TLS server or `wss` promise,
compression/extension negotiation, fragmented WebSocket messages, Stream dependency, scheduler,
router, or application framework.
