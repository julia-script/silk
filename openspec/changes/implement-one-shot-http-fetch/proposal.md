## Why

Silk's streaming HTTP client exposes the ownership and completion boundaries needed for correct
HTTP, but applications still have to compose route selection, pooling, redirects, content decoding,
metadata copying, destination backpressure, and final connection disposition themselves. A bounded
one-shot facade is needed so that convenient fetches do not hide unbounded collection, silent body
discard, provisional metadata, or reuse of an incompletely consumed connection.

## What Changes

- Add an ordinary-source `silk.http_fetch` actor with semantic `FetchRequest`, admitted
  `FetchOptions`, immutable `ResponseMetadata`, owned `FetchResult` and `CollectedResponse`, and
  bounded phase-aware `FetchError` values.
- Expose streaming-sink, bounded-collection, and explicit-discard destinations for each of the four
  delivered redirect source contracts: Empty, borrowed repeatable Bytes, affine OneShot, and
  ReplayFactory. The twelve source-specific operations share one private destination engine while
  preserving exact source, sink, error, and requirement rows.
- Copy final URI, status, ordered wire headers, and representation metadata before body reads; copy
  final trailers only after verified completion. Stream sequential borrowed chunks with
  backpressure, publish collections atomically, and make discard an explicit bounded destination.
- Compose the existing streaming client, proxy routing, bounded connection pool, redirect policy,
  and response-content decoder through an immutable client context. Manual, Raw, Fresh, and Direct
  modes independently disable their corresponding automatic behavior.
- Enforce distinct finite head, framing, encoded, decoded, delivered, metadata, buffer, collection,
  and discard budgets under one unchanged optional absolute deadline, with checked accounting and
  no unlimited sentinel.
- Preserve precise component and generic source/sink failures, report known destination progress,
  treat completed HTTP error statuses as results, reject switching responses, and release or evict
  every affine owner exactly once without cleanup reads.
- Register the actor and add only compact feature-specific structured and shared-corpus evidence,
  reusing existing parser, codec, proxy, pool, TLS, and transport vectors rather than adding another
  worker, compiled program, or protocol matrix.

## Capabilities

### New Capabilities

- `one-shot-http-fetch`: Bounded one-shot HTTP request composition, source-specific request-body
  authority, sink/collect/discard destinations, owned final metadata, policy modes, finite limits,
  deadline and failure semantics, lifecycle rules, and portable evidence.

### Modified Capabilities

None. The streaming client, content, redirect, proxy, and pool contracts are delivered by active
stack items rather than archived main capability specs, so their composition is specified within
the new fetch capability.

## Impact

The change adds canonical `silk.http_fetch` source and its standard-library registration surfaces,
plus the narrow native/context composition needed to select pooled or fresh direct/proxy attempts.
It reuses `silk.http_client`, `silk.http_content`, `silk.http_redirect`, `silk.http_proxy`, and
`silk.http_connection_pool` as their respective owners; it does not add a compiler primitive,
second body-source abstraction, parser, codec, transport, resolver, TLS stack, browser Fetch model,
cookie jar, cache, background worker, or retry engine. Verification is confined to existing HTTP
analysis and shared acceptance boundaries, with one compact source reused for the single intended
LLVM-to-Wasm leg and no broad local pipeline or documentation-generation run. Tracking:
https://linear.app/juliaortiz/issue/JUL-200.
