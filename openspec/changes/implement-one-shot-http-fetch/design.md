## Context

See `proposal.md` for motivation and `specs/one-shot-http-fetch/spec.md` for the behavioral
contract. The delivered HTTP stack already separates the necessary owners:

- `http_client` owns request/response exchange state, final-head selection, deframed raw reads,
  response-bound content decoding, trailer completion, one request deadline, and reuse eligibility.
- `http_redirect` owns semantic request transitions and exposes four source-specific scoped response
  operations: Empty, Bytes, OneShot, and Replay. Its delivered API intentionally has no singular
  `BodySource` value because Silk cannot infer absent source types or avoid irrelevant conformance
  evidence in such a union.
- `http_proxy` owns route selection, request-target form, proxy credentials, CONNECT, and per-origin
  route recomputation. `http_connection_pool` owns conservative connection identity, compatible
  checkout, exact-once idle publication/eviction, and immutable acquisition context.
- `http_content`, `http_response`, `http_headers`, and `uri` own representation selection and the
  bounded copies needed to keep final metadata after a live response is released.

The Linear text predates the source-specific redirect revision and names a singular
`HttpRedirect.BodySource`. Reintroducing that type would contradict the same ticket's prohibition
on a second body-source abstraction and would weaken the delivered exact-row design. This change
therefore maps all three destinations across the four delivered redirect operations while sharing
one private destination engine.

## Goals / Non-Goals

**Goals:**

- Offer convenient one-shot completion without hiding response-body ownership, unbounded storage,
  implicit discard, or connection disposition.
- Preserve exact generic source and sink rows and higher-ranked nonescape across redirect, attempt,
  response, chunk, and producer boundaries.
- Make route, pooling, content, destination, deadline, and every finite budget independently
  selectable and observable.
- Reuse lower-level owners directly, adding only the context/acquisition bridge required to compose
  them.
- Keep feature evidence within existing expensive compiler/runtime boundaries.

**Non-Goals:**

- A browser Fetch API, CORS, cookies, cache, automatic authentication discovery, ambient proxy
  lookup, automatic retry, parallel racing, background work, or a dependency on affine Stream.
- Protocol switching, WebSocket or CONNECT-duplex handoff, HTTP/2, connection coalescing, a new
  parser/content decoder, or another socket/TLS implementation.
- Transactional external sink effects, preemption of arbitrary CPU callbacks, or stronger cleanup
  guarantees than the underlying affine providers.
- A compatibility alias for the obsolete singular body-source design.

## Decisions

### Destination × source entry points preserve the delivered redirect contract

Expose twelve public operations:

- `toSinkEmpty`, `toSinkBytes`, `toSinkOneShot`, and `toSinkReplay`;
- `collectEmpty`, `collectBytes`, `collectOneShot`, and `collectReplay`;
- `discardEmpty`, `discardBytes`, `discardOneShot`, and `discardReplay`.

Each thin source wrapper calls exactly one matching `http_redirect` scoped-response operation. Empty
has no source generic or source rows; Bytes borrows one immutable repeatable payload; OneShot moves
one producer and its precise error/requirement family; Replay moves one factory and preserves both
factory and producer families. `FetchRequest` holds only semantic request data, so it neither owns
nor existentially hides a source.

All wrappers install a destination policy and delegate the final response to one private delivery
engine. The engine is generic over a destination handler that accepts the immutable metadata and
borrowed chunks, then publishes its destination-specific output after finish. A private lexical
service may erase the concrete handler shape from the fixed redirect response handler, following
the selected-body technique already established by `http_redirect`; it must remove only its private
requirement and keep public error/requirement rows exact.

Alternative: reproduce the stale `BodySource<OneShot,Factory>` union. Rejected because Empty and
Bytes cannot infer absent types, unconditional bounds demand dummy witnesses, and it creates the
second source abstraction the ticket forbids. Alternative: one operation with boxed/existential
dispatch. Rejected because it erases affine source authority and precise generic rows. Alternative:
duplicate a complete engine per source/destination. Rejected because it multiplies state machines
and compiler specializations.

### One semantic request is prepared afresh by existing owners

`FetchRequest` borrows validated URI and ordered `Headers`, records an explicit method and existing
request policy, and offers `get(uri)` for GET with empty caller headers. Source selection is an
operation argument, never a method heuristic. Each redirect attempt lets existing request actors
generate Host, route-specific target form, framing, connection policy, Expect, authentication, and
Accept-Encoding. Fetch never accepts unchecked strings, patches serialized bytes, or stores a
`PreparedRequest` across origins.

The admitted `FetchOptions` contains redirect, content, pool, proxy, deadline, and finite storage
policies. Admission happens once before acquisition. Cross-field checks use checked arithmetic,
require positive working buffer capacity, and retain independent caps rather than deriving one
unbounded aggregate.

Alternative: infer POST when a nonempty source is supplied. Rejected because source existence does
not establish method intent and would make redirect behavior depend on convenience inference.

### A fetch-client boundary adapts immutable context to redirect attempts

Add the smallest target-neutral fetch client interface needed by the actor. Its higher-ranked
attempt operation receives the current redirect attempt, the unchanged deadline, `PoolMode`,
`ProxyMode`, and a nonescaping attempt handler. An internal adapter implements
`http_redirect.AttemptClient` by closing over the admitted modes. This keeps policy orchestration in
`http_fetch` while leaving actual route/acquisition ownership beside the client implementation.

The native context owns or boundedly copies immutable provider identity, HTTP limits/version,
configured proxy policy and credential identity, optional compatible pool handle, TLS/ALPN policy,
and frozen trust source/snapshot as defined by the delivered clients. For each current redirect
origin it:

1. chooses Direct or recomputes the inherited configured proxy route;
2. chooses Fresh or compatible pool checkout according to `PoolMode` and context availability;
3. prepares origin-form, absolute-form, or CONNECT-followed-by-origin-form bytes through the route
   and request owners;
4. authenticates TLS against the original current origin for direct HTTPS or tunneled HTTPS; and
5. lends one active exchange to the higher-ranked handler under the original deadline.

`UseContextPool` has the specified context-dependent default: use a compatible explicit pool when
one exists and otherwise take the defined Fresh path. Selecting `Fresh` always bypasses the pool.
Selecting `Direct` always bypasses configured proxy routing. Neither choice permits environment
lookup or plaintext fallback.

Alternative: make Fetch open native sockets directly. Rejected because scripted and Wasm providers
need a target-neutral boundary and acquisition ownership already belongs to client/pool actors.
Alternative: pass separate mutable trust, proxy, and pool knobs per request. Rejected because those
values are part of authenticated connection identity and could make pool reuse unsafe.

The immutable native context must remain reusable across callback result and error families. Its
`FetchClient` conformance therefore binds those parameters in the interface application rather than
storing phantom per-invocation parameters in `Context`. Conformance admission must follow the
prescriptive IMPL-006 rule: every binder is determined by the complete interface/provider head.
Parameters absent from both sides remain invalid, overlap still compares complete heads, and
conditional requirements must still descend through the provider. This does not introduce runtime
dispatch or generic interface-operation syntax.

### Final metadata is copied once and moved into the result

Inside the final redirect callback, copy the final current URI and response head under one admitted
metadata budget, then derive a separately owned representation description from the selected
content plan. Construct one immutable `ResponseMetadata` owner before the first body read and lend
the same view to each sink callback. Do not allocate or mutate a trailer slot inside that value.

After the content decoder and framing reach terminal completion, copy the final trailers under the
remaining metadata budget, finish the response, and move the metadata owner plus trailers and
counters into `FetchResult`. Ordered wire headers remain unchanged. Representation metadata records
the content mode/codings and completed representation facts instead of rewriting wire fields such
as Content-Length.

Metadata accounting sums payload and index storage for URI, headers, and trailers with checked
arithmetic. Allocator bookkeeping is excluded because it is provider-specific. The response, URI,
head, and chunk borrows stay inside the final callback; only independently owned copies cross it.

Alternative: expose the live response head to the sink. Rejected because body reads invalidate or
advance the response owner. Alternative: append trailers to metadata already shared with earlier
callbacks. Rejected because it makes an allegedly immutable historical view temporally false.

### One delivery loop drives sink, collector, and discard policies

The private delivery engine begins Raw or Decode content through the existing response-bound client
operation and repeatedly requests at most `maxBufferedBytes` into one positive-capacity scratch
buffer. The content/client actors retain wire, encoded, decoder, decoded, framing, and trailer
accounting; Fetch adds checked delivered and destination accounting.

For sink delivery, the handler receives metadata and a borrowed chunk, is awaited once, and only on
success advances the delivered total. A typed destination-failure value retains the original sink
error together with the earlier successful total and current offered length. It does not estimate
external partial effects.

For collection, a private bounded Bytes builder implements the same destination interface. It
checks both `maxBodyBytes` and remaining allocation before reserve/copy, never trusts Content-Length
as an allocation command, and moves the body into `CollectedResponse` only after whole-operation
success. Failure drops the private builder and publishes no partial response.

For discard, a zero-storage destination advances only the delivered count and enforces
`maxDiscardBytes`; it still drives decoder, framing, trailers, and finish. Cap failure is terminal
and evicts. Neither collection nor discard calls a special drain path, so all three destinations
share the same completion semantics.

Alternative: implement discard by returning immediately after the head. Rejected because unread or
unvalidated response state is not a successful one-shot result. Alternative: reuse redirect's
intermediate bounded drain. Rejected because that operation deliberately permits cap-and-continue,
whereas final discard must prove completion. Alternative: preallocate from Content-Length. Rejected
because the field is untrusted and may describe coded rather than delivered bytes.

### Content policy composes without changing provenance

Raw selects the existing raw representation path and passes coded bytes after HTTP deframing;
unknown coding names remain metadata rather than errors. Decode installs the existing content plan
and supports only its configured coding stack, windows, owned bytes, checksum, and completion
rules. Accept-Encoding generation is requested from the content/request owner only for Decode, only
for configured supported codings, and only when absent from caller headers.

The engine never sniffs bytes or retries a codec. Method/status rules that forbid a body bypass
decoding and must still reach verified empty completion. Separate wire, encoded, intermediate,
decoded, delivered, destination, metadata, and scratch bounds are retained rather than collapsed,
because each limits a different attacker-controlled expansion.

Alternative: normalize decoded metadata by rewriting received headers. Rejected because callers
need both wire provenance and representation facts and raw Content-Length is not decoded length.

### One operation deadline and phase context survive all owners

The admitted optional absolute deadline is passed unchanged through fetch client, route, pool,
connect, TLS, request producer, every redirect attempt, head/body/trailer I/O, sink callbacks, and
finish. Existing per-phase caps may clamp it but never renew or extend it. `None` remains explicit
unbounded waiting, not a numeric sentinel. Examples choose a concrete deadline so convenience code
does not accidentally demonstrate indefinite network waits.

The fetch actor adds bounded safe phase/progress context at URI, Route, Connect, Request, Redirect,
Head, Decode, Destination, and Finish boundaries while retaining precise underlying component or
generic source/sink failures. Generic causes remain typed payloads or direct alternatives; they are
never converted to strings or `unknown`. Context owns only safe URI/status/counter data and never
credentials, body bytes, or live response borrows.

HTTP status alone is not failure: completed 4xx/5xx values publish normally. Final 101 and
successful CONNECT are rejected before any duplex authority could escape. Explicit finish/flush or
shutdown errors remain normal typed failures.

Alternative: reset a duration at every redirect. Rejected because slow chains would exceed the
caller budget. Alternative: flatten all component failures into one message. Rejected because it
destroys recovery and progress distinctions.

### Cleanup abandons or evicts but never performs hidden I/O

Nest source/factory, redirect attempt, response, decoder, collector, and pool/connection brackets so
one owner is armed at each acquisition and disarmed only when authority is published to the next
owner. On success, finish and existing client/pool eligibility decide reuse. On typed failure,
defect, or cancellation, automatic nonparking cleanup drops private buffers, refunds reservations,
and closes or evicts active transport authority exactly once. It never reads, drains, retries,
flushes, or gracefully shuts down the network.

Cleanup close failures are suppressed and cannot replace the protected success or failure, matching
the existing duplex/pool guarantee. Explicit finish failures remain observable. A streamed prefix
may already be externally visible, but collection result ownership is not published until success.

Alternative: best-effort drain on sink failure. Rejected because cleanup could park, hide the
original failure, exceed the deadline, or accidentally publish a connection whose decoder never
completed.

### Verification extends existing compiler and runtime boundaries

Add fetch declarations, exact generic-row witnesses, pure phase-valid option/metadata evaluations,
and ownership diagnostics to the existing HTTP analysis fixtures. Reuse one positive realized
snapshot and at most the existing consolidated frontend-negative boundary; do not create a worker
or duplicate realization for one source.

Add one compact table/scenario to the shared native acceptance corpus. It combines compatible pool
reuse, a relative redirect, explicit proxy routing, and compressed final content, then carries only
the distinct destination, mode, cap, 404, switching, progress, failure, cancellation, and
exact-release signals. Reuse that exact source for one named LLVM-to-Wasm leg. Reuse established
native HTTP/HTTPS, parser, codec, proxy, pool, TLS, trust, socket, and framing evidence rather than
building another matrix.

Registration, generated catalog artifacts, public doc comments, the canonical reference page, and
examples are updated narrowly as tracked artifacts. Do not run or add whole-stdlib doc generation,
the full local pipeline, stress/timing/memory checks, fresh-process checks, or per-case compilation
as feature evidence.

Alternative: add a fetch-specific native binary test for every mode. Rejected because the shared
corpus already supplies differential native execution and each extra source pays the compiler
pipeline again.

## Risks / Trade-offs

- [Twelve public operations look broad] -> keep them as thin exact-row adapters over four delivered
  redirect sources and one private destination engine; the width is the cost of avoiding phantom
  witnesses and erased affine authority.
- [A generic destination service accidentally erases sink rows] -> model lexical provision after
  the redirect selected-body service, subtract only the private requirement, and assert exact
  signatures in one structured snapshot.
- [Metadata copying exceeds the cap after output began] -> admit and copy URI/head before the first
  chunk, account trailers against the remaining budget, report the later failure as provisional
  stream failure, and publish collections only after completion.
- [A discard path accidentally becomes cleanup drain] -> implement discard as a normal selected-body
  destination inside the live response, with a required cap and the same verified finish path.
- [Mode composition opens the wrong security context] -> seal native context construction, pass
  explicit pool/proxy modes into one per-hop adapter, use route recomputation, and leave TLS identity
  and trust with the existing acquisition owner.
- [Feature evidence amplifies compiler memory] -> one positive analysis boundary, one consolidated
  negative boundary, one compact shared runtime source, one reused Wasm leg, and no new worker,
  program matrix, protocol matrix, or broad local pipeline.

## Migration Plan

This is a green-field actor stacked after JUL-197, JUL-198, and JUL-199. Add the final source-specific
API, context bridge, registrations, generated artifacts, documentation, and bounded evidence
together; update any in-repository examples directly to the final surface. Rollback removes those
artifacts and the narrow fetch-client conformance. There is no persisted state, old API,
compatibility shim, or data migration.
