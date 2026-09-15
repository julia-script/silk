## Why

Silk's streaming HTTP client can retain a complete HTTP/1 connection, but native convenience calls
close that owner after every callback. Applications need one bounded, synchronized owner that can
reuse eligible plain or authenticated TLS connections without retaining stale request deadlines,
mixing security contexts, exceeding concurrent-open capacity, or weakening affine cleanup.

## What Changes

- Add an ordinary-source bounded HTTP connection-pool actor with fail-fast global and per-origin
  capacity, finite idle retention, deterministic recent-match selection and oldest-idle eviction,
  lazy expiry, and explicit scoped closure.
- Bind each pool and every fallibly copied handle to one immutable acquisition and security context,
  including transport/provider representation, HTTP limits and version policy, routing identity,
  TLS/ALPN verification policy, proxy credential identity, and one frozen trust snapshot.
- Add the smallest native owned-acquisition seam needed to publish a complete direct TCP, Unix, or
  HTTPS HTTP connection into the pool without borrowing through the existing callback API.
- Separate acquisition, request, handshake, drain, and idle deadlines. Pooled HTTP owners retain no
  acquisition or previous-request deadline, while every exchange receives the caller's unchanged
  absolute request deadline.
- Add client-owned reuse eligibility and bounded explicit drain operations so only fully completed,
  persistent, suffix-free connections reenter idle storage; every abandoned, exhausted, upgraded,
  close-delimited, failed, or stale owner closes outside shared state.
- Preserve precise generic result, failure, and requirement channels through checkout, exchange,
  source acquisition, and cancellation-safe release; expose no escaping connection or lease borrow.
- Register and document the actor and add bounded structured and shared runtime evidence without a
  new worker, stress/timing suite, transport matrix, fresh-process test, or full CI/pipeline task.

## Capabilities

### New Capabilities

- `bounded-http-connection-pooling`: Finite local HTTP/1 connection pooling, immutable security
  context, reservation and lease ownership, reuse eligibility, deadline separation, idle eviction,
  explicit drain, pool closure, typed failures, and portable delivery evidence.

### Modified Capabilities

None. The relevant HTTP client, native acquisition, proxy, TLS, trust, and local Shared contracts
are currently delivered by in-flight changes rather than archived capability specs; their narrow
pooling seams are specified within this new capability.

## Impact

The change adds a standard-library pool actor, manifest/generated documentation surfaces, focused
client reuse/drain queries, and an owned native acquisition boundary. It composes the existing
`silk.http_client`, native client, TLS connection, trust snapshot, proxy route, monotonic clock, and
local `Shared` actors. Direct pooling does not depend on proxy delivery; when proxy routes are
available, only their sealed route identity is admitted. The change does not add a compiler
primitive, threaded mutex, waiter queue, background worker, automatic request retry, HTTP/2,
connection coalescing, Stream dependency, or WebSocket pooling. Tracking:
https://linear.app/juliaortiz/issue/JUL-197.
