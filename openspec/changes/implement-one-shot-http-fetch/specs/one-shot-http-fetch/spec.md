## Purpose

Define a bounded one-shot HTTP facade that composes Silk's streaming client policies while keeping
request-body authority, destination effects, metadata ownership, resource release, and every
storage and time limit explicit.

## ADDED Requirements

### Requirement: Fetch requests are semantic and body authority is source-specific

The standard library SHALL provide `silk.http_fetch` with `Fetch`, `FetchRequest`, `FetchOptions`,
`ResponseMetadata`, `FetchResult`, `CollectedResponse`, and `FetchError`. `FetchRequest` SHALL borrow
a validated URI and ordered validated request headers, retain an explicit method, and contain no
prepared wire bytes, transport state, or request-body owner. `FetchRequest.get(uri)` SHALL construct
an explicit GET request with empty caller headers. Supplying content to an operation SHALL NOT
implicitly change the request method to POST.

For each destination, Fetch SHALL expose separate Empty, borrowed-repeatable-Bytes, affine-OneShot,
and effectful-ReplayFactory operations: `toSinkEmpty`, `toSinkBytes`, `toSinkOneShot`,
`toSinkReplay`; `collectEmpty`, `collectBytes`, `collectOneShot`, `collectReplay`; and
`discardEmpty`, `discardBytes`, `discardOneShot`, `discardReplay`. The source-specific operations
SHALL follow the delivered redirect source contract exactly and SHALL expose only the source
errors, requirements, and conformance evidence they can use. Replay SHALL acquire a fresh scoped
producer for every retained-body attempt including attempt zero. OneShot authority SHALL remain
affine and SHALL NOT be restored after an attempted write or hidden by buffering. Fetch SHALL NOT
introduce a singular body-source sum, dummy source witnesses, or another source abstraction.

Host, request-target form, framing, origin authentication, proxy authentication, and
Accept-Encoding SHALL be generated only through their existing owning actors. Fetch SHALL NOT
accept unchecked URI/header strings or reverse-parse prepared request bytes.

#### Scenario: Source rows remain exact

- **WHEN** callers type-check Empty, Bytes, OneShot, and Replay operations for the same semantic
  request
- **THEN** Empty and Bytes require no producer/factory evidence, OneShot exposes only its producer
  rows, Replay exposes its factory and producer rows, and no operation requires a placeholder source

#### Scenario: Content does not select POST

- **WHEN** a caller supplies repeatable bytes to a request whose explicit method is GET
- **THEN** the attempted request remains GET and body handling follows the redirect method/body
  policy without an implicit POST transition

### Requirement: Sink delivery is sequential, borrowed, and provisional

Each `toSink*` operation SHALL invoke its effectful sink sequentially with immutable
`ResponseMetadata` and one borrowed current byte chunk. The chunk borrow SHALL NOT escape the
callback. A successful callback SHALL mean that the complete offered chunk was consumed; Fetch
SHALL await that success before reading or offering another chunk. `deliveredBodyBytes` SHALL
advance only after a successful callback.

If a sink fails, the failure SHALL retain the caller's precise typed cause and requirement row and
SHALL report the total bytes accepted by earlier successful callbacks plus the current offered
length. It SHALL NOT claim how much of the current chunk an external destination changed. Bytes
already accepted by a sink SHALL remain externally visible after a later sink, checksum, framing,
decoder, transport, trailer, or finish failure; streamed output is provisional until whole-operation
success and is not transactional.

#### Scenario: Tiny sink applies backpressure

- **WHEN** a response is delivered through a sink that accepts small chunks and suspends before each
  success
- **THEN** Fetch offers no later chunk until the current callback succeeds and the final delivered
  count equals the sum of the successfully offered chunks

#### Scenario: Sink failure reports only known progress

- **WHEN** earlier callbacks accepted seven bytes and the sink fails while offered a four-byte chunk
- **THEN** the destination failure retains the original sink error, reports seven completed bytes
  and an offered length of four, and makes no claim about partial external effects within that chunk

### Requirement: Final metadata is owned, ordered, bounded, and temporally truthful

Fetch SHALL copy the final URI, status, ordered wire response headers, and separate representation
metadata before body reads invalidate the final head. That immutable header-only view SHALL be the
`ResponseMetadata` lent to every sink callback. It SHALL NOT contain or later mutate to include
trailers. After verified representation, framing, and trailer completion, `FetchResult` SHALL own
the final URI, status, ordered wire headers, final ordered trailers, representation metadata,
redirect count, encoded body count, and delivered body count, and SHALL contain no body buffer.
`CollectedResponse` SHALL own one `FetchResult` and one collected `Bytes` body.

Wire headers SHALL retain their received meaning and order. A decoded representation view SHALL be
separate and SHALL NOT present a wire Content-Length as decoded or delivered length. Owned metadata
accounting SHALL include URI, header, and trailer payload plus index storage and SHALL exclude only
allocator bookkeeping. Publishing metadata beyond `maxOwnedMetadataBytes` SHALL fail before an
oversized owner escapes.

#### Scenario: Headers and trailers outlive the response owner

- **WHEN** a response with duplicate ordered headers and final trailers completes successfully
- **THEN** its result retains those ordered values after response and connection release, while the
  metadata previously shared with the sink remains header-only

#### Scenario: Decoded view does not rewrite wire metadata

- **WHEN** Decode delivers a representation whose byte length differs from the wire Content-Length
- **THEN** wire headers preserve the received Content-Length and separate representation metadata
  reports decoding and completed delivered length truthfully

### Requirement: Collection publishes atomically within an explicit body cap

Each `collect*` operation SHALL use the same final-response delivery path as sink and discard,
require `maxBodyBytes`, and return a `CollectedResponse` only after verified whole-operation
completion. Zero SHALL permit only an empty selected representation. Fetch SHALL check the body cap
before allocation growth or writing beyond it, use checked arithmetic, and SHALL NOT reserve an
untrusted Content-Length beyond the remaining admitted budget. On any error, Fetch SHALL release
the partial buffer and return no partial `CollectedResponse`; this publication guarantee SHALL NOT
claim to undo network effects.

The collection cap SHALL remain distinct from encoded, decoded, delivered, metadata, and working
buffer limits, and the earliest applicable bound SHALL fail without excess storage or delivery.

#### Scenario: Collection cap fails before the ninth byte

- **WHEN** the selected representation has nine bytes and `maxBodyBytes` is eight
- **THEN** collection returns a destination limit failure, allocates or writes no ninth body byte,
  publishes no `CollectedResponse`, and leaves the connection ineligible for reuse

#### Scenario: Zero collection accepts only empty content

- **WHEN** `maxBodyBytes` is zero
- **THEN** an empty completed representation succeeds and any nonempty chunk fails before being
  stored

### Requirement: Discard is an explicit bounded validating destination

Each `discard*` operation SHALL require `maxDiscardBytes`, consume the selected Raw or Decode
representation through the same delivery and completion path, and return only `FetchResult` after
content, framing, and trailers verify. The discard cap SHALL count delivered representation bytes;
wire/framing and encoded/decoded limits SHALL remain independently effective. Zero SHALL permit only
an empty representation.

Reaching the discard cap before completion SHALL fail and make the connection ineligible rather
than returning success from the head. It SHALL NOT inherit redirect intermediate-drain semantics,
where a cap may close one intermediate response and continue. Fetch SHALL expose no implicit or
cleanup-time drain; callers that need head-only handling SHALL use the streaming client directly.

#### Scenario: Discard cap does not convert truncation into success

- **WHEN** discard reaches its delivered-byte cap before the selected representation completes
- **THEN** the operation fails, performs no further cleanup read, and closes or evicts the active
  connection

#### Scenario: Decode discard still verifies the decoder

- **WHEN** all decoded bytes fit the discard cap but the coded stream fails checksum or completion
  validation
- **THEN** discard returns the precise decoder failure and does not publish a successful result

#### Scenario: All destinations agree on a completed response

- **WHEN** the same scripted completed response is sent to a tiny sink, bounded collection, and
  bounded discard under otherwise identical options
- **THEN** all three publish identical status and final metadata semantics, sink chunks and the
  collected body contain the same delivered representation bytes, and discard reports that same
  delivered count without storing a body

### Requirement: Options compose independently selectable policies and finite budgets

`FetchOptions` defaults SHALL be redirect Manual, content Raw, proxy InheritContext, and pool
UseContextPool when the explicitly supplied context contains a compatible pool and otherwise Fresh.
A default client context SHALL use Direct routing and perform no environment lookup. Manual, Raw,
Fresh, and Direct SHALL independently disable redirect following, decoding, pool use, and configured
proxy routing respectively; disabling one SHALL NOT silently disable or enable another.

Options SHALL carry finite admitted head/parser/framing/trailer budgets,
`maxEncodedBodyBytes`, `maxDeliveredBodyBytes`, `maxOwnedMetadataBytes`, positive
`maxBufferedBytes`, the existing content decoding limits when Decode is selected, and the existing
redirect limits when Follow is selected. There SHALL be no unlimited numeric sentinel. Zero byte
budgets SHALL permit only empty corresponding content; the working output buffer SHALL have positive
capacity. All counters, cap combinations, and narrowing SHALL use checked arithmetic.

#### Scenario: Modes disable behavior independently

- **WHEN** Manual, Raw, Fresh, or Direct is selected while the other context facilities remain
  available
- **THEN** only that selected redirect, decode, pool, or proxy behavior is disabled, with no ambient
  fallback

#### Scenario: Invalid options fail before contact

- **WHEN** a buffer capacity is zero, a finite bound is invalid, or combined accounting overflows
- **THEN** option admission returns the precise local failure before route selection, pooling,
  resolution, trust loading, or transport contact

### Requirement: Client context owns routing, pooling, transport security, and ambient authority

Fetch SHALL use one explicitly supplied immutable client context to select Direct or configured
proxy routing and Fresh or compatible pooled acquisition for every redirect attempt. InheritContext
SHALL use only that context's proxy configuration; Direct SHALL bypass it without environment
lookup. Route selection SHALL be recomputed for every current redirect origin. Pool reuse SHALL use
the delivered conservative connection identity and eligibility rules; incompatible or unavailable
pool composition SHALL fail or use Fresh only when the selected pool mode explicitly permits that
defined fallback.

TLS verification, ALPN, transport-provider identity, trust policy, and proxy credential identity
SHALL belong to the immutable context. A request SHALL NOT override verification or change a pool's
authenticated identity. Fetch SHALL perform no additional ambient environment, filesystem, trust,
time, or entropy lookup except work documented by an explicitly selected underlying provider or
trust source. Unsupported target, route, or transport composition SHALL fail before contact and
SHALL NOT fall back to plaintext.

#### Scenario: Redirect recomputes route without leaking credentials

- **WHEN** a followed redirect changes origin and the configured routing policy changes from proxy
  to bypass or back to proxy
- **THEN** Fetch recomputes the route for each origin, prepares the correct request-target form, and
  never emits route-specific Proxy-Authorization as an origin header or into a tunnel

#### Scenario: Fresh and pooled acquisition remain distinguishable

- **WHEN** a compatible context pool exists but the caller selects Fresh
- **THEN** the operation opens a fresh owner and does not check out or publish that owner through the
  pool

### Requirement: Content selection preserves raw and decoded provenance

Raw mode SHALL deliver received coded representation bytes after HTTP message deframing and SHALL
preserve wire content metadata, including unsupported coding names. Decode mode SHALL support
exactly the delivered content actor's coding set, order, limits, checksums, and completion rules and
SHALL fail an unsupported coding. Fetch SHALL generate Accept-Encoding only for configured supported
codings, only in Decode mode, and only when the caller supplied no Accept-Encoding field.

Fetch SHALL NOT sniff coding, retry with a different codec, decode content forbidden by request
method or response status, or merge wire and representation accounting. Wire/framing, encoded,
intermediate decoder, decoded, delivered, destination, metadata, and working-buffer caps SHALL remain
distinct so compression or framing overhead cannot evade a bound.

#### Scenario: Unknown coding differs by mode

- **WHEN** a completed response names an unsupported content coding
- **THEN** Raw delivers the coded bytes and preserves the name while Decode fails with the precise
  unsupported-coding error before reporting success

#### Scenario: Accept-Encoding has one owner

- **WHEN** Decode is configured with supported codings and the request omits Accept-Encoding
- **THEN** Fetch asks the content/request owners to generate the admitted field exactly once, while a
  caller-supplied field is preserved without duplicate generation

### Requirement: One absolute deadline and precise failures span the complete operation

The optional absolute operation deadline SHALL default to `None`, explicitly permitting indefinite
waiting. Fetch SHALL pass the same value, never a per-hop reset, through route selection, pool
checkout, connection acquisition, TLS handshake, request production, redirects, head parsing,
content delivery, sink waits, trailers, and explicit completion. Provider-specific shorter limits
MAY clamp individual phases but SHALL NOT extend the operation deadline. The deadline SHALL NOT
claim to preempt synchronous resolution, trust loading, or arbitrary callback CPU work.

A completed HTTP 4xx or 5xx response SHALL be a successful result when its selected body and
trailers complete. A final 101 response or successful CONNECT response SHALL fail
`UnsupportedSwitchingResponse` and close or evict because the one-shot facade does not transfer the
duplex. Malformed messages, unsupported route/transport, timeout, decoder/checksum, allocation,
limit, and finish failures SHALL remain distinct.

`FetchError` SHALL carry bounded phase context from URI, Route, Connect, Request, Redirect, Head,
Decode, Destination, or Finish plus only known progress and safe owned context. It SHALL retain the
precise underlying component or generic source/sink failure without stringifying, erasing it to
`unknown`, or exposing credentials or borrowed response state.

#### Scenario: Deadline is not renewed by redirect or pool reuse

- **WHEN** route, acquisition, an intermediate redirect, final body delivery, and sink waits consume
  one operation timeline
- **THEN** every phase observes the original absolute deadline and no redirect or reused connection
  grants a new full interval

#### Scenario: Completed 404 is not an exchange failure

- **WHEN** a 404 response has a valid completed selected representation and trailers
- **THEN** Fetch returns a successful result with status 404 and its owned final metadata

#### Scenario: Switching response cannot escape

- **WHEN** the final response is 101 or a successful CONNECT tunnel
- **THEN** Fetch returns `UnsupportedSwitchingResponse`, transfers no duplex authority, and makes the
  connection ineligible for pooling

### Requirement: Affine resources release exactly once without hidden network cleanup

Fetch SHALL scope the active response, request-body producer or replay factory loan, content
decoder, owned destination buffer, connection owner or pool lease, and reservation so that each is
released exactly once on success, typed failure, defect, or cancellation within the existing
scoped-cleanup guarantee. Explicit finish, flush, or shutdown operations MAY fail normally.
Automatic cleanup SHALL be nonparking, SHALL suppress close failures rather than replace the
protected success or error, and SHALL never read, drain, retry, or gracefully shut down the network.

Sink cancellation, destination cap failure, checksum/framing failure after delivery, abandoned
response state, and producer failure SHALL conservatively close or evict the active connection.
Closing a client context with live pool leases SHALL follow the pool's established lease rules.
Fatal traps remain outside the underlying scoped-cleanup guarantee.

#### Scenario: Failure and cancellation release every owner once

- **WHEN** producer, sink, decoder, transport, finish, or cancellation interrupts any acquisition or
  delivery phase
- **THEN** every published resource and reservation is released once, the primary outcome is
  preserved, and no cleanup read or retry occurs

#### Scenario: Successful pooled completion publishes only an eligible owner

- **WHEN** the final response, selected content, trailers, and finish all complete on a persistent
  connection below its request budget
- **THEN** only the pool/client owners decide it is eligible for idle publication; Fetch does not
  duplicate or override their state machine

### Requirement: Delivery evidence and documentation remain bounded and compositional

Delivery SHALL register and document the target-neutral actor and supported native composition,
including source-specific operations, options and defaults, provisional streaming, collection and
discard bounds, metadata timing, content provenance, deadlines, errors, release, proxy/pool/redirect
composition, examples with explicit deadline/provider/trust choices, exclusions, and deliberate
differences from the pinned Zig fetch implementation.

One existing HTTP structured-analysis boundary SHALL prove public reachability, exact generic rows,
pure configuration/metadata StaticEvaluation where phase-valid, and affine nonescape diagnostics.
One compact scripted integration in the existing shared native acceptance corpus SHALL combine
compatible pool reuse, relative redirect, explicit proxy routing, and compressed final content and
shall contain the distinct sink, collect, discard, mode, limit, 404, switching, error, cancellation,
and release signals. That exact source SHALL provide exactly one intended LLVM-to-Wasm leg for
target-neutral behavior. Existing parser, codec, proxy, TLS, trust, socket, and native suites SHALL
supply their established lower-level vectors.

The change SHALL NOT add a test worker, separately compiled program per mode/case, fresh-process,
stress, timing, memory, browser, CORS, cookie, WebSocket, retry, or duplicate TLS/socket/proxy/codec
matrix. Whole-repository pipelines and whole-stdlib documentation generation SHALL NOT be added as
implementation evidence.

#### Scenario: One shared integration proves the composed facade

- **WHEN** the compact fetch source runs in shared native acceptance and its one named Wasm leg
- **THEN** it proves the distinct fetch outcomes without duplicating lower-level protocol matrices
  or compiling one program per scenario

#### Scenario: Documentation examples expose finite policy

- **WHEN** a caller follows the canonical HTTP/HTTPS examples
- **THEN** provider and trust choices, an absolute deadline, destination cap, content mode, redirect
  mode, pool mode, and proxy mode are explicit rather than ambient or unbounded
