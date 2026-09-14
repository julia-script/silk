## Why

The shared HTTP value, head, body-framing, buffered-byte, and native-listener actors still do not
define a server exchange lifecycle. Without one, handlers can accidentally reinterpret unread body
or pipelined bytes, send multiple final responses, lose exact progress after short writes, or reuse a
connection whose message boundary was never completed.

## What Changes

- Add a scoped, serial streaming HTTP/1.0 and HTTP/1.1 connection API over an exclusive concrete
  `ByteDuplex`, with bounded buffered input/output, one active request loan, and explicit connection
  states.
- Add request-body reads and bounded explicit discard using the shared HTTP body decoder, including
  exact suffix preservation, typed truncation/framing failures, and `100-continue` behavior that
  never waits for an unsupported or rejected expectation body.
- Add informational and final response operations using the shared head serializer and body
  encoder, enforcing response uniqueness, special-status body exclusions, fixed/chunked/close
  framing, short-write progress, and explicit finish.
- Add persistence policy for HTTP/1.0 and HTTP/1.1, request-count and informational limits, close
  token precedence, unread-body closure, graceful finite drain, and terminal cancellation/output
  failure semantics.
- Add scoped Upgrade and CONNECT tunnel handoff that preserves the exact unread buffered suffix and
  consumes HTTP ownership without returning a detached transport.
- Add a native one-connection admission adapter over the scoped listener API; it performs no
  background accepting, routing, or unbounded task spawning.
- Document and verify the portable scripted server lifecycle plus native admission boundary at the
  cheapest structural and shared-runtime tiers.

## Capabilities

### New Capabilities

- `streaming-http-server`: Scoped HTTP/1.0 and HTTP/1.1 server connections, request/response
  streaming, persistence, expectations, shutdown, upgrade/tunnel handoff, and serial native
  admission.

### Modified Capabilities

None.

## Impact

- Adds ordinary-source `silk.http_server` and `silk.http_server_native` standard-library modules and
  their generated registrations.
- Builds on `silk.http`, `silk.http_head`, `silk.http_body`, `silk.buffered_duplex`,
  `silk.byte_duplex`, and the native listener/accepted-connection actors without duplicating their
  parsing, framing, buffering, or descriptor ownership.
- Adds focused compiler/ownership tests, one portable shared acceptance source, a controlled native
  admission fixture, and public reference documentation.
- Establishes the server exchange contract consumed by WebSocket upgrade and frame/message work;
  native TLS serving, routers, supervisors, and background accept queues remain outside this change.
