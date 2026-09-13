## Why

HTTP transports can split a head at any byte or return body bytes in the same read. The client and
server need one bounded parser that reports exact consumption, preserves every suffix byte, and
publishes only validated borrowed metadata; otherwise each transport risks losing or interpreting
wire bytes differently.

## What Changes

- Add a `silk.http_head` actor with affine request and response parsers, explicit finite limits,
  exact per-call progress, reusable allocated storage, and typed poisoning/reset semantics.
- Parse the strict HTTP/1.0 and HTTP/1.1 request-line, status-line, and header profile selected by
  JUL-194, delegating values, targets, Host validation, and independent ownership to the shared HTTP
  value actors delivered by JUL-193.
- Add mutation-atomic complete-head size calculation and serialization into caller-owned memory;
  body framing, transport, persistence, content decoding, and trailers remain separate.
- Register and document the new public actor and add focused structured/runtime evidence for split
  points, exact consumption, validation offsets, ownership, limits, reuse, and serialization.

## Capabilities

### New Capabilities

- `http-head-parsing`: Bounded incremental HTTP/1.x request/response head parsing, borrowed head
  publication, parser reset/poisoning, and atomic memory serialization.

### Modified Capabilities

None.

## Impact

The change adds an ordinary-source standard-library module under
`packages/compiler/stdlib/silk/`, a manifest registration and generated public surface, focused
compiler fixtures and shared native/Wasm acceptance evidence, and HTTP reference documentation.
It depends on the shared HTTP values already delivered by JUL-193 and unblocks JUL-195 without
introducing socket, TLS, buffered-I/O, framing, or Stream dependencies.
