## Why

Silk's streaming HTTP client deliberately returns redirect responses as ordinary response values,
leaving applications to rebuild requests, replay bodies, sanitize credentials, and release each
intermediate connection correctly. A shared bounded policy is needed so redirect transitions do
not reuse consumed producers, leak authority-sensitive headers, reset deadlines, or leave borrowed
response state outside its owner.

## What Changes

- Add an ordinary-source `silk.http_redirect` actor with validated `RedirectPolicy`, bounded
  redirect history and URI storage, explicit Follow/Manual behavior, and source-specific scoped
  response operations that lend exactly one final response to their callback.
- Add a rebuildable semantic redirect request plus separate empty, borrowed-repeatable-byte,
  affine-one-shot, and effectful-replay-factory operations. Each operation exposes only the source
  errors, requirements, and witnesses it can actually use; replay factories create a fresh scoped
  producer for every attempt.
- Implement the exact 301/302/303/307/308 method/body matrix, URI-reference resolution and RFC 9110
  fragment inheritance, method-aware loop detection, hop/history/URI limits, and distinct typed
  redirect failures.
- Enforce origin and downgrade policy before contact, strip hop-by-hop and authority-sensitive
  headers, recompute request framing and proxy routes per hop, and never retry transport failures
  or copy route-specific proxy authentication as an origin header.
- Close intermediate responses by default or use an explicitly bounded drain policy under the one
  unchanged absolute operation deadline, preserving source, callback, client, URI, and allocator
  error/requirement channels and releasing each acquired resource exactly once.
- Register and document the actor and add compact structured/native/Wasm evidence that reuses the
  existing HTTP client, URI, content-framing, proxy, TLS, and transport surfaces.

## Capabilities

### New Capabilities

- `bounded-http-redirects`: Bounded redirect policy, rebuildable requests, replay-source contracts,
  URI and loop handling, origin/header transitions, intermediate-response cleanup, deadline and
  ownership rules, typed failures, and delivery evidence.

### Modified Capabilities

None. The reusable attempt-client seam and its composition with the streaming HTTP client and proxy
routes are specified inside the new redirect capability because those capabilities do not yet have
archived main specs to amend.

## Impact

The change adds canonical `silk.http_redirect` source and generated manifest/documentation surfaces.
It adds only the narrow reusable attempt-client adapter needed to drive the existing
`silk.http_client` exchange and `silk.http_proxy` route APIs; it does not add another parser,
transport, resolver, TLS implementation, retry engine, cookie jar, cache, pool, or dependency on the
higher-level affine Stream actor. Existing URI and HTTP request actors remain the owners of
resolution and wire preparation. Evidence is limited to one shared analysis snapshot, one compact
shared native corpus program, and exactly one intended LLVM-to-Wasm leg, with no stress, timing,
fresh-process, or duplicate TLS matrix. Tracking: https://linear.app/juliaortiz/issue/JUL-198.
