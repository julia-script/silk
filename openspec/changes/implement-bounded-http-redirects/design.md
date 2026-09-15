## Context

See `proposal.md` for motivation and `specs/bounded-http-redirects/spec.md` for the behavioral
contract. The existing client already owns staged request writes, final-head selection, bounded
framing discard, content completion, one absolute exchange deadline, and connection reuse evidence.
`http_request.PreparedRequest` intentionally owns only serialized bytes and admission identity; it
does not retain the method, URI fragment, source headers, or header policy needed to construct a
different request. URI resolution is lossless but intentionally implements RFC 3986 only, so the
HTTP-specific fragment inheritance rule belongs above it. The proxy work provides sealed route
selection and `Route.recompute` plus scoped `withRoute` acquisition.

The redirect feature must compose those owners while keeping every response and body producer
inside its acquisition scope. It also needs a transport-independent attempt boundary so later pool
and fetch actors can reuse the policy without coupling redirects to native sockets.

## Goals / Non-Goals

**Goals:**

- Represent enough semantic request state to rebuild every hop without parsing emitted request
  bytes or weakening request admission.
- Make repeatability a type- and policy-visible capability, with exactly one scoped producer per
  attempted request and no hidden whole-body buffering.
- Complete validation and sanitization before each acquisition, preserve the one caller deadline,
  and leave every intermediate scope before the next begins.
- Keep the attempt boundary reusable by native, proxy-aware, pooled, and eventual fetch contexts
  while preserving all generic success, error, and requirement rows.

**Non-Goals:**

- Transport retry, parallel racing, cookie/referrer/cache policy, permanent-redirect caching,
  browser URL behavior, IDNA, a new content decoder, connection pooling, or a dependency on the
  higher-level affine Stream actor.
- Compatibility aliases, a second prepared-request representation, or a compiler-known redirect
  operation.

## Decisions

### A rebuildable request is distinct from `PreparedRequest`

`silk.http_redirect` owns a `Request` value that borrows the initial `Uri`, `Method`, `Headers`, and
`HeaderPolicy` and retains the HTTP version, continue policy, request/header limits, and explicit
head/credential budgets needed by `http_request`. It is immutable input to the operation. Internal
`AttemptRequest` state owns the current URI, transformed method, sanitized headers, and derived body
mode for one hop. Each acquisition prepares a fresh `PreparedRequest` from that state and the
current route.

This keeps semantic redirect policy outside the wire serializer while preserving the serializer as
the only authority for Host, target form, framing, Expect, and connection fields. It also avoids
adding unrelated getters or borrowed source state to `PreparedRequest`.

Alternative: reverse-parse `PreparedRequest.bytes()` after every response. Rejected because it
cannot recover fragment absence, caller policy, header provenance, or affine body authority and
would duplicate the parser and serializer contracts.

### Body sources separate first-attempt ownership from replay capability

Define a generic `BodySource<'bytes, OneShot, Factory>` union:

- `Empty` selects empty framing and needs no producer.
- `RepeatableBytes` borrows immutable bytes for the complete operation and creates a zero-resource
  cursor for every retained-body attempt.
- `OneShot` moves one affine producer into operation state and permits exactly one producer scope.
- `ReplayFactory` moves an effectful factory whose higher-ranked callback lends a fresh producer for
  each attempt, including attempt zero.

A small `BodyProducer` interface supplies selected `BodyMode` metadata and pulls bounded byte
chunks. A factory uses callback-scoped acquisition rather than returning an unconstrained producer,
so file handles or other resources cannot escape and release failures can be bracketed without
manual cleanup branches. The shared writer loops on `http_client.writeSome`, records observed bytes,
then calls `finishRequest`; selected framing, declared known length, and observed length must agree.
The factory and producer retain separate generic error and requirement parameters, and those rows
flow through `withResponse` without wrapping.

When a transition drops content, no producer/factory call occurs. When it retains content, Empty,
RepeatableBytes, and ReplayFactory are available; OneShot after attempt zero fails before the next
acquisition. An early final does not restore OneShot authority because external producer effects may
already have occurred even when the writer reports no progress.

Alternative: buffer a OneShot body on its first use. Rejected because it changes memory bounds,
duplicates caller storage, cannot roll back source effects, and makes replayability depend on timing.

### The attempt client is a higher-ranked acquisition boundary

The redirect actor defines an `AttemptClient` interface and a callback-style `AttemptHandler`. One
call receives the current independently owned URI, immutable semantic request fields, body mode,
unchanged deadline, and one scoped producer. It selects/recomputes the route, prepares the request
through `http_request`, acquires a connection, drives `withExchange`, and lends the active exchange
to the handler. The handler's response/exchange borrow is higher-ranked and cannot escape.

`Redirect.withResponse(client, request, policy, bodySource, deadline, callback)` remains generic over
the client, producer, factory, and final callback. Its return channel is the direct union of redirect,
URI/request/client/acquisition/source errors and the callback's `E`; its requirement row is their
direct union with the callback's `R`. No `unknown` erasure or boxed dynamic dispatch is introduced.

The proxy-aware adapter stores the immutable proxy configuration or selected initial route and
uses `Route.recompute` for every current origin. Direct/Tunnel attempts prepare origin-form requests;
Forward attempts call `prepareForward`. Tunnel acquisition reuses `withRoute`, original-origin TLS,
and caller-supplied trust acquisition. The native adapter belongs beside existing native client
acquisition rather than inside the pure redirect policy. Later pooling can implement the same
interface without changing redirect rules.

Alternative: let `withResponse` open native sockets directly. Rejected because it would hard-code a
provider, impede deterministic scripted evidence, and create a second acquisition path for pooling
and fetch.

### One iterative state machine separates intermediate and final scopes

`withResponse` owns an internal `State` containing current owned URI, current method, owned sanitized
headers, current body disposition, hop count, and bounded history. Each loop iteration invokes one
attempt-client bracket. Inside the attempt handler it receives the final response head and returns
one of two nonescaping outcomes:

- Final: invoke the user callback immediately with the live exchange, an owned URI whose borrowed
  view is limited to the callback, and hop count.
- Redirect: copy and validate everything needed for `NextState`, apply previous-response policy, and
  return that owned state out of the attempt bracket.

The next loop iteration begins only after the attempt bracket and producer bracket both finish, so
no response, connection, producer, or temporary URI borrow overlaps the next acquisition. Manual and
non-selected status paths take the Final branch and do not inspect Location.

Alternative: recursively call the next attempt from inside the previous response callback. Rejected
because it nests live leases, retains unread response ownership across destinations, and makes hop
cleanup and cancellation ordering difficult to prove.

### URI transition uses RFC 3986 resolution plus an HTTP fragment step

While the head is borrowed, copy the one Location value into bounded scratch and parse it as a
`UriReference`. Reserve `base length + reference length + 1` using checked arithmetic, matching the
URI resolver's conservative bound, and ensure both raw and final serializations fit `maxUriBytes`.
Call `Uri.resolveInto` without changing the current owner. If the reference fragment is absent and
the current URI fragment is present, append that fragment to separately bounded output and parse
the final bytes again; a present empty fragment bypasses inheritance. Convert the successful text to
`OwnedUri` before releasing the head.

Build a history key from transformed method, normalized `Origin`, and exact encoded path/query.
Normalize only scheme/host/default port and empty path-to-slash; keep percent-escape spelling and
exclude fragment. Store keys and their owned bytes in one prevalidated bounded history allocation,
checking a candidate before acquisition. Method is part of the key so POST-to-GET is not a false
loop.

Alternative: compare complete URI strings. Rejected because fragments do not identify HTTP request
targets and equivalent host/default-port spellings would evade loops.

### Sanitization rebuilds one owned header set before contact

An internal sanitizer walks the original/current `Headers` once, first collecting names nominated
by all Connection fields under the existing header limits. It constructs one `OwnedHeaders` output
while applying three filters:

1. Always remove hop-by-hop/framing fields and Connection-nominated extensions.
2. When content is dropped, remove every content/framing field named by the specification.
3. On cross-origin transitions, retain only the fixed safe set plus configured safe custom names,
   then unconditionally subtract credentials, configured sensitive names, Origin, and Referer.

All name-list admission is case-insensitive, bounded, and overlap-checked. Host, target, framing,
connection fields, and route-specific Proxy-Authorization are generated later by their owning
actors. Validation, origin/downgrade checks, replay availability, history insertion, and preparation
capacity are complete before the attempt client can perform contact.

Alternative: mutate the caller's header storage or permit allowlist precedence. Rejected because it
would make failure non-atomic and allow credentials to survive an authority change.

### Bounded drain gets an outcome, not an ambiguous swallowed error

Close policy simply returns from the intermediate exchange without finishing it, allowing existing
scope finalization to revoke reuse and close. Drain policy needs to distinguish the allowed
`CapReached` outcome from malformed framing, transport, timeout, or allocation failure. Add the
smallest sibling operation to `http_client` that drives the existing decoder with an aggregate wire
ceiling and returns `Completed | CapReached`; it does not reinterpret protocol failures or publish
reuse. On Completed, redirect handling calls `finishResponse`; on CapReached, it leaves the exchange
incomplete so the owner closes, then may proceed. The drain deadline is clamped against the same
operation deadline before calling the client.

This avoids catching a broad `ClientError.Body` and guessing whether it represents the selected cap.
Cleanup is bracketed so release failure cannot replace a policy, source, callback, or transport
outcome.

Alternative: call existing `discardRemaining` and continue after every failure. Rejected because it
would hide malformed framing and read failures that must terminate the redirect chain.

### Failure context is owned once at the redirect boundary

`RedirectError` contains the redirect-specific reason plus bounded owned hop/status/current/next URI
context appropriate to the failure. It never stores response borrows, request credentials, body
bytes, proxy tokens, or configured opaque identity payloads. Underlying URI, request, client,
acquisition, producer/factory, allocator, and callback errors remain their original alternatives in
the public effect signature instead of being converted to strings or `unknown`.

### Verification shares expensive compiler boundaries

Add redirect declarations and policy assertions to one shared `Analysis` snapshot in the existing
HTTP test worker. Use StaticEvaluation for transition, URI, header, history, and limit claims. Add one
compact table-driven redirect source to the existing native acceptance corpus for the distinct
writer/replay/cleanup/deadline signals, then reuse that exact source for one named LLVM-to-Wasm leg.
Derive its wire expectations independently from RFC 9110/3986 and record the deliberate differences
from the pinned Zig implementation alongside the fixture. The proxy/TLS suites already prove
transport identity; redirect evidence asserts recomputation and emitted request bytes without
another socket or certificate matrix.

## Risks / Trade-offs

- [Higher-ranked factory/client rows expose a compiler limitation] -> model them after the existing
  route/client handlers, prove signatures in structured analysis first, and keep runtime cases in
  the one shared corpus source.
- [Sanitization accidentally retains credentials or duplicate framing] -> use deny-last precedence,
  route/request-owned generation, and exact emitted-byte assertions for authority changes.
- [Fragment inheritance mutates or aliases the current URI] -> resolve into separate bounded storage,
  apply inheritance only after inspecting presence, and adopt only after final parse succeeds.
- [Intermediate cleanup double-releases or masks the cause] -> make each iteration one nested
  producer/attempt bracket and carry the primary outcome across finalization.
- [Redirect fixtures amplify compiler memory use] -> keep one analysis snapshot, one native program,
  one reused Wasm leg, and no per-case compilation, stress, timing, fresh-process, or TLS matrix.

## Migration Plan

This is a new green-field actor. Add the module, narrow client adapter, registrations, generated
artifacts, reference, and evidence together; update all in-repository callers directly to the final
surface. Rollback removes those artifacts and the narrow drain/attempt seams. No persisted data,
legacy API, compatibility path, or migration shim exists.
