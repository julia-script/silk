## Why

Silk now has owned native sockets, bounded buffering, HTTP head/body framing, and authenticated TLS, but no client that ties their lifetimes and completion boundaries together. JUL-23 delivers incremental HTTP exchanges without hidden collection, replay, or reuse of incomplete messages.

## What Changes

- Add ordinary-source `silk.http_client` connection and exchange actors with exclusive scoped loans, staged uploads/downloads, finite budgets, informational responses, and explicit CONNECT handoff.
- Add origin and request admission, generated header policy, strict explicit Basic authorization, and native HTTP/HTTPS composition preserving origin identity and absolute deadlines.
- Reuse the delivered TLS external-deadline pump and synchronous resolver refusal contract rather than introduce another transport path.
- Deliver structured ownership evidence, deterministic native/Wasm acceptance, generated registrations and reference documentation.

## Capabilities

### New Capabilities

- `streaming-http-client`: scoped plain/TLS HTTP/1 client, request admission, framing, continuation, completion, and tunnel handoff.
- `http-basic-authorization`: bounded atomic Basic encoding and explicit origin-bound authentication policy.

### Modified Capabilities

None. The existing owned TLS change already supplies external absolute deadlines.

## Impact

New standard-library actors compose `http`, `http_head`, `http_body`, `base64`, `network_address`, `resolver`, `native_socket`, and `tls_connection`. Tests reuse compiler analysis and shared native acceptance. No retries, redirects, pooling, new content codecs, WebSocket protocol, or automatic body collection are introduced. Tracking: https://linear.app/juliaortiz/issue/JUL-23.

The exchange also binds the delivered content-decoding core to its own response head and wire body, enforcing one decoding selection before raw consumption.
