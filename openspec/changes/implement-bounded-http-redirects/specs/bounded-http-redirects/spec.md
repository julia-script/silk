## Purpose

Define bounded, replay-aware HTTP redirect handling whose URI transitions, credentials, deadlines,
intermediate responses, and final borrowed response have one explicit scoped owner.

## ADDED Requirements

### Requirement: Redirect policy is explicit, validated, and finite

The standard library SHALL provide an ordinary-source redirect actor with `Manual` and `Follow`
modes. Defaults SHALL be Manual, 10 maximum hops, 8192 maximum bytes for each raw or resolved URI,
90112 maximum history bytes, cross-origin Deny, downgrade Deny, 301/302 POST Preserve, and previous
response Close. The policy SHALL own bounded lists of permitted cross-origin custom header names and
sensitive header names, default both lists empty, reject overlap, and validate all counts, byte
capacities, and checked arithmetic before network I/O.

Manual SHALL return the first response unchanged. Follow SHALL select only 301, 302, 303, 307, and
308; every other status, including 304, SHALL be final. A selected redirect with no remaining hop
capacity SHALL fail HopLimit rather than returning the response or contacting another destination.
The operation SHALL NOT mutate the caller's initial URI or provide a permanent-redirect cache.

#### Scenario: Manual preserves an otherwise invalid redirect

- **WHEN** Manual receives status 302 with missing, duplicate, or invalid Location fields
- **THEN** it lends that first response to the final callback without applying redirect policy

#### Scenario: Zero-hop Follow is finite

- **WHEN** Follow with maximum hops zero receives an otherwise valid 301 response
- **THEN** it returns HopLimit and performs no next-hop contact

### Requirement: Method and body transitions follow the selected status contract

For 301 and 302, redirect handling SHALL preserve method and body by default; explicit ToGet policy
SHALL change only exact uppercase POST to GET and drop its body. For 303, it SHALL preserve HEAD and
otherwise use GET, dropping the body. For 307 and 308, it SHALL preserve method and body and require
replay capability. Extension methods SHALL otherwise be preserved exactly.

When content is dropped, redirect handling SHALL remove Content-Length, Transfer-Encoding,
Content-Type, Content-Encoding, Content-Language, Content-Location, Digest, Content-Digest,
Repr-Digest, Trailer, and Expect and SHALL regenerate framing through the streaming client.

#### Scenario: POST becomes body-free GET after 303

- **WHEN** a POST with content receives a selected 303 response
- **THEN** the next request is GET, its content headers are removed, and its body source is not run

#### Scenario: HEAD and extension methods retain their defined semantics

- **WHEN** HEAD receives 303 or a non-POST extension method receives 301 under ToGet policy
- **THEN** the next method remains respectively HEAD or the exact extension method

### Requirement: Body replay never fabricates repeatability

The redirect actor SHALL expose body sources for Empty, RepeatableBytes, affine OneShot, and
ReplayFactory. RepeatableBytes SHALL borrow caller storage for the complete redirect operation.
OneShot SHALL be consumed at most once and SHALL return ReplayUnavailable before a retained-body
next-hop contact, even when an early response arrived before observed upload progress. Empty SHALL
require no replay.

ReplayFactory SHALL create a fresh scoped producer for every attempt, including the first, while
preserving the factory's and producer's generic failures and requirements. Every produced source
SHALL match the selected framing and declared length, and its observed bytes SHALL satisfy that
length; a mismatch SHALL return ReplayContractMismatch. The library SHALL treat equivalent content
as the caller's promise and SHALL NOT buffer or compare complete bodies, guess external offsets, or
copy an affine producer. Each producer scope SHALL be released after its attempt or failure without
undoing already accepted external effects.

#### Scenario: One-shot content cannot cross a 307

- **WHEN** a 307 follows an attempt using a OneShot body
- **THEN** redirect handling returns ReplayUnavailable and emits no request to the next target

#### Scenario: Factory creates one scoped producer per attempt

- **WHEN** a two-hop chain retains content through ReplayFactory
- **THEN** factory and release counters record one fresh producer and one release for each attempt,
  with framing and observed-length contracts checked independently

### Requirement: Location resolution and loop detection are bounded and lossless

For a selected Follow response, redirect handling SHALL require exactly one Location field: absence
SHALL fail LocationMissing and duplicates SHALL fail LocationAmbiguous. Empty Location SHALL remain
a valid URI reference. Before contact it SHALL bound and copy raw Location while the response head
is live, validate percent escapes, resolve relative references against the current URI through the
standard URI actor, and validate the independently stored result as HTTP or HTTPS with a host,
effective port, no userinfo, and no capacity overflow. It SHALL preserve encoded path/query spelling.

When the reference has no fragment, redirect handling SHALL inherit the current fragment; a present
empty fragment SHALL replace it. Fragments SHALL remain final-URI metadata and SHALL NOT enter the
wire request target.

The history key SHALL contain the transformed method, normalized origin, and exact encoded
path/query, treating an empty path as `/` and excluding the fragment. Origin equality SHALL use
canonical host identity, scheme, and effective default port. Redirect handling SHALL store at most
maximum hops plus one keys within the history budget and SHALL reject a repeated key before sending.

#### Scenario: Resolve and inherit fragments without changing the request target

- **WHEN** a response redirects from `https://example/a#old` to `../b?x` and then to `#`
- **THEN** the first resolved URI inherits `old`, the second has an explicitly empty fragment, and
  neither fragment appears in either emitted target

#### Scenario: Loop keys include transformed method but exclude fragment

- **WHEN** POST at one URI changes through 303 to GET at that URI and a later redirect repeats GET
  at a fragment-only variant
- **THEN** the first GET is not a false loop and the repeated GET fails RedirectLoop before contact

### Requirement: Origin changes sanitize authority-sensitive state before contact

An origin SHALL be normalized scheme, canonical host identity, and effective port; subdomain and
port changes SHALL be cross-origin. Cross-origin Deny SHALL fail RedirectOriginDenied before next
contact. Allow SHALL recompute Host, target, physical endpoint, HTTPS identity and SNI, and proxy
route for the next origin. HTTPS-to-HTTP SHALL additionally require downgrade Allow and otherwise
fail DowngradeDenied before contact. No proxy or transport failure SHALL fall back to another route.

Every hop SHALL remove Connection, Keep-Alive, TE, Trailer, Transfer-Encoding, Upgrade,
Proxy-Connection, and each header named by the prior Connection field. Proxy-Authorization SHALL
never be copied as an origin header. A cross-origin hop SHALL always strip Authorization, Cookie,
Proxy-Authorization, all configured sensitive headers, Origin, and Referer. No allowlist entry SHALL
override those removals. It SHALL retain only Accept, Accept-Encoding, Accept-Language, User-Agent,
explicitly permitted safe custom headers, and content type/encoding/language fields required by a
retained body. It SHALL NOT add DNS-suffix credential rules, Location-derived credentials, cookies,
or implicit referrer policy.

#### Scenario: Cross-origin allowlist cannot retain credentials

- **WHEN** an allowed cross-origin redirect names Authorization, Cookie, or a configured sensitive
  field in the safe custom allowlist
- **THEN** those fields are absent from emitted bytes and all next-hop validation completes before
  destination contact

#### Scenario: Downgrade requires both permissions

- **WHEN** an HTTPS response redirects to HTTP with cross-origin Allow but downgrade Deny
- **THEN** redirect handling returns DowngradeDenied without route acquisition or request output

### Requirement: Redirect attempts compose the existing client and proxy owners

Redirect handling SHALL use an immutable rebuildable semantic request rather than reverse-parsing a
serialized prepared request. A reusable attempt-client boundary SHALL acquire exactly the route and
connection selected for the current URI, rebuild a prepared request through the existing request
actor, drive the existing exchange/body operations, and lend one scoped response. Every new origin
SHALL recompute policy through the active immutable proxy configuration. Direct and tunneled
requests SHALL use origin-form targets; Forward SHALL use the proxy actor's absolute-form path;
route-specific proxy authentication SHALL remain owned by that route.

The public redirect operation SHALL call its final callback exactly once with the final scoped
response, independently owned final URI, and hop count. It SHALL preserve arbitrary callback
success, failure, and requirement channels, and neither the borrowed response nor a URI view SHALL
escape the owner implicitly. It SHALL NOT implement another parser, resolver, transport, TLS pump,
automatic retry, concurrent attempt, or connection pool.

#### Scenario: Proxy bypass is recomputed on every hop

- **WHEN** a proxied origin redirects to an exact bypass origin and then to another proxied origin
- **THEN** the three attempts use respectively the selected proxy route, direct route, and freshly
  selected proxy route without copying proxy credentials into origin headers

#### Scenario: Final borrows remain scoped

- **WHEN** a callback tries to retain or duplicate the final response outside its operation scope
- **THEN** structured analysis rejects the escape while ordinary callback failures remain unwrapped

### Requirement: Intermediate cleanup and deadlines have one protected outcome

The next URI, method, body replay capability, headers, route, and limits SHALL be validated before
the next request is sent. Each intermediate response SHALL leave its scoped connection bracket
before the next acquisition. Close policy SHALL perform no drain. Drain policy SHALL delegate to
the existing bounded framing discard, count all consumed wire bytes including chunk extensions and
trailers, and clamp its finite deadline to the operation's unchanged optional absolute deadline.
Reaching the discard cap SHALL close that connection and may continue; an actual framing, protocol,
read, timeout, or cancellation failure SHALL stop the chain.

The same absolute deadline SHALL cover every attempt and cooperative wait and SHALL never reset per
hop. Timeout, cancellation, source failure, callback failure, and policy failure SHALL release each
active producer, response, lease, and transport exactly once. Cleanup failure SHALL NOT replace the
protected primary outcome. Reuse authority SHALL be possible only after complete framing and the
existing eligibility checks.

#### Scenario: Drain cap closes and continues but malformed framing stops

- **WHEN** one intermediate response exceeds its discard cap and another has malformed framing
- **THEN** the first connection closes before the next attempt and the second terminates the chain
  with its framing failure rather than being treated as a successful drain

#### Scenario: One deadline spans the chain

- **WHEN** earlier redirects consume most of a finite absolute deadline
- **THEN** later attempts receive that same deadline and timeout without a renewed per-hop budget

### Requirement: Redirect failures own bounded context and ship portably

Redirect policy SHALL distinguish LocationMissing, LocationAmbiguous, LocationInvalid,
RedirectSchemeDenied, RedirectOriginDenied, DowngradeDenied, HopLimit, RedirectLoop,
ReplayUnavailable, ReplayContractMismatch, and LimitExceeded with bounded relevant hop, status, and
URI context. Existing client, URI, allocator, transport, source, and callback failures SHALL remain
precise rather than being erased to an unknown channel. Failed Follow SHALL NOT return a successful
redirect response.

The manifest, generated source table, public API documentation, and prescriptive reference SHALL
expose the target-neutral actor. Structured analysis and static evaluation SHALL prove pure policy,
URI, ownership, and error-row claims; one compact shared native corpus and one intended
LLVM-to-Wasm leg SHALL prove the distinct runtime transitions. Wire expectations SHALL be derived
independently from the governing RFCs, and the reference SHALL record deliberate differences from
the pinned Zig implementation. The capability SHALL NOT require a higher-level Stream dependency,
new TLS/socket matrix, browser policy, stress benchmark, timing assertion, fresh-process check, or
per-scenario compilation.

#### Scenario: Policy failure returns owned bounded evidence

- **WHEN** a selected Location exceeds a configured limit after an intermediate response is read
- **THEN** the operation closes the response and returns LimitExceeded with bounded owned context
  and no dangling response or URI view

#### Scenario: Target-neutral redirect behavior agrees across engines

- **WHEN** the shared scripted redirect program runs natively and through its one intended Wasm leg
- **THEN** both produce the same method, target, header, replay, loop, and release signals
