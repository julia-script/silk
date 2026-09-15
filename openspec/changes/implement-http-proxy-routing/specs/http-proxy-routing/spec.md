## Purpose

Define explicit, bounded HTTP forward-proxy routing that keeps proxy identity and credentials
separate from the origin while safely composing CONNECT tunnels with origin-authenticated TLS.

## ADDED Requirements

### Requirement: Proxy configuration is explicit, immutable, and bounded

Canonical ordinary-source `silk.http_proxy` SHALL expose owned `ProxyConfig`, `ProxyAuth`, and
`BypassPolicy` values plus opaque `ProxyConfigId` and `ProxyAuthContextId` values. Programmatic
configuration construction SHALL accept only a plain proxy host and port from 1 through 65535 and
caller-supplied nonsecret configuration/authentication-context IDs. It SHALL copy and validate at
most 64 exact bypass origins, at most 4096 encoded authentication-token bytes, and an explicit
`maxOwnedConfigBytes` covering copied hosts, credentials, entries, and index capacities. All size
arithmetic SHALL be checked, and invalid syntax or insufficient capacity SHALL fail before
resolution, connection, authentication generation, or wire output. Configuration SHALL NOT read
environment variables, PAC files, system settings, or another ambient credential source.

An optional URI convenience constructor SHALL admit only a plain `http` authority with no
userinfo, path beyond empty, query, or fragment. An `https` proxy URI SHALL return
`UnsupportedProxyTransport` during construction. SOCKS, proxy chains, reverse proxies, and
TLS-encrypted proxy endpoints SHALL remain unrepresented as successful configurations, routes, or
acquisition inputs.

#### Scenario: Reject capacity before contact

- **WHEN** copied proxy configuration exceeds an entry, credential, or owned-byte limit or overflows
  checked size arithmetic
- **THEN** construction returns a typed configuration failure without DNS, connection, trust,
  authentication generation, or transport output

#### Scenario: Parse only a plain proxy authority

- **WHEN** the convenience constructor receives an `https` URI
- **THEN** it returns `UnsupportedProxyTransport` before producing a configuration or performing any
  resolution or connection work

#### Scenario: Reject malformed plain proxy configuration

- **WHEN** the convenience constructor receives userinfo, path, query, fragment, an empty host, or a
  port outside 1 through 65535
- **THEN** it returns the corresponding configuration failure rather than stripping components or
  selecting another route

### Requirement: Prepared proxy authentication is canonical and secret-safe

`ProxyAuth` SHALL be either `None` or an explicitly supplied nonempty prepared Basic token. A
prepared token SHALL use the canonical strict standard-padded Base64 validator and SHALL reject
noncanonical padding, an invalid alphabet, whitespace, controls, and nonzero pad bits before
contact. The 4096-byte encoded limit implies a maximum decoded length of 3072 bytes. Validation
SHALL allocate no decoded scratch and SHALL expose no separate unreachable scratch-limit failure.
It SHALL NOT reinterpret username or password character sets. Digest, NTLM, Negotiate, ambient
credential discovery, and automatic 407 retry SHALL NOT be provided.

The caller SHALL supply nonsecret `u64` payloads for opaque configuration and
authentication-context identities. The identity values SHALL be `Copy`, SHALL preserve their exact
equality across configuration and route copies, and SHALL NOT be derived implicitly from proxy
endpoint or secret bytes. The caller SHALL provide a different authentication-context identity when
the credential or its authorization context changes. Formatting and typed diagnostics SHALL expose
neither identity payloads nor secret token bytes. Bypass selection SHALL occur before proxy
credentials are copied into request scratch or a `Proxy-Authorization` field is produced.

#### Scenario: Reject a malformed prepared token

- **WHEN** a nonempty prepared Basic token contains URL-safe bytes, whitespace, controls, malformed
  padding, nonzero unused pad bits, or exceeds the encoded-token limit
- **THEN** configuration fails before any proxy contact and exposes no secret bytes in the error

#### Scenario: Bypass without touching credentials

- **WHEN** an origin matches an exact bypass entry
- **THEN** route selection returns a direct route without producing proxy authentication bytes

#### Scenario: Keep identity stable without hashing credentials

- **WHEN** a configuration and its selected route are copied
- **THEN** their configuration/authentication identities compare exactly equal to the caller-supplied
  originals without hashing, formatting, or inspecting the prepared token

### Requirement: Route selection implements the admitted matrix without fallback

`selectRoute(config, origin)` SHALL be pure and SHALL return a direct route for an exact bypass
match, a Forward route for an HTTP origin through a plain HTTP proxy, and a Tunnel route for an
HTTPS origin through a plain HTTP proxy. Exact bypass comparison SHALL include scheme, normalized
host identity, and effective port; it SHALL use equivalent canonical numeric-address forms and
SHALL NOT perform suffix, wildcard, CIDR, or DNS-result matching. Equal proxy and origin host text
SHALL NOT implicitly bypass an explicitly configured proxy.

A successful route SHALL retain immutable proxy host and port, opaque caller-supplied configuration
and authentication-context identity, original origin, Forward or Tunnel mode, and origin security
context. Because configuration admits only plain proxy endpoints, route selection and acquisition
SHALL expose no encrypted-proxy variant. A proxy failure SHALL never select a direct route,
plaintext origin connection, or retry automatically.

#### Scenario: Select every admitted route

- **WHEN** route selection receives an HTTP origin, an HTTPS origin, and an exact bypass origin
  under one plain proxy configuration
- **THEN** it returns respectively Forward, Tunnel, and Direct routes with the original origin and
  immutable route identity intact

### Requirement: Routed request admission separates logical origin from physical peer

The streaming HTTP client SHALL expose one explicit routed-request admission path used by proxy
policy. It SHALL accept distinct logical-origin and physical-peer origins plus an admitted Forward or
CONNECT mode. Logical-origin validation SHALL govern the request target, `Host`, and origin
`Authorization`; physical-peer identity SHALL govern which connection may send the prepared bytes.
A prepared routed request SHALL fail client admission on any connection whose physical peer differs.

The existing direct request preparation path SHALL continue to set logical origin and physical peer
to the same value and SHALL reject caller `Proxy-Authorization`. The routed path SHALL accept proxy
credentials only as proxy-policy-owned prepared input, SHALL reject the same field in caller headers,
and SHALL NOT make a routed request admissible on the origin-authenticated connection inside a
tunnel.

#### Scenario: Admit one forward request on its proxy peer

- **WHEN** proxy policy prepares an absolute-form request for logical origin `http://example.com`
  and physical peer `http://proxy.example:8080`
- **THEN** the client admits it only on that proxy-peer connection while `Host` and origin credential
  checks remain bound to `example.com`

#### Scenario: Preserve direct credential rejection

- **WHEN** ordinary direct preparation receives caller `Proxy-Authorization`
- **THEN** it fails before output and exposes no route token or alternate direct preparation path

### Requirement: Forward routes send one origin-bound absolute-form request

An HTTP Forward route SHALL connect to the proxy endpoint through routed request admission and
serialize the original URI as an
absolute-form request target, excluding userinfo and fragment, writing `/` for an empty path, and
preserving accepted encoded path and query spelling. `Host` SHALL identify the origin rather than
the proxy. `Proxy-Authorization` SHALL be generated only from the selected proxy configuration;
caller-supplied overrides or duplicate fields SHALL fail before output. Origin `Authorization`
SHALL remain subject to origin policy and SHALL NOT be reused as proxy authentication.

The proxy leg SHALL be described as plaintext. No API or documentation SHALL imply that origin TLS
protects its request target, headers, or proxy credentials.

#### Scenario: Forward an HTTP URI

- **WHEN** `http://example.com/a?b` is sent through a configured plain proxy
- **THEN** the proxy receives `GET http://example.com/a?b HTTP/1.1`, `Host: example.com`, and only
  the configured proxy credential field

#### Scenario: Reject a proxy credential override

- **WHEN** caller headers contain `Proxy-Authorization` in addition to or instead of configured
  proxy authentication
- **THEN** preparation fails before writing any request byte

### Requirement: CONNECT reveals only an authority and accepts any final 2xx

An HTTPS Tunnel route SHALL resolve and connect only the proxy endpoint, then send a body-free
CONNECT request whose request target and `Host` contain the original origin host and explicit
effective port, including brackets around IPv6 literals. CONNECT preparation SHALL NOT resolve the
origin or include origin `Authorization`, Cookie, path, query, fragment, upload bytes, or another
origin header.

CONNECT response handling SHALL reuse the shared incremental head contract. Each head SHALL be
bounded to 32768 bytes and 100 fields; at most 8 informational responses and 65536 aggregate
informational head bytes SHALL be accepted. Status 101 SHALL be an invalid transition. Any final
status from 200 through 299 SHALL establish the tunnel immediately after the head terminator and
SHALL ignore `Content-Length` and `Transfer-Encoding` on that successful response.

A 407 response SHALL return `ProxyAuthenticationRequired` owning the status/reason and every
`Proxy-Authenticate` field, including duplicates in original order. Other non-2xx responses SHALL
return `ProxyRejected` owning the status/reason and every field in original order. Both copies SHALL
use limits of 100 fields, 256 name bytes, 8192 value bytes, 32768 aggregate field bytes, and 32768
total owned bytes. Total-owned accounting SHALL include field-record storage as well as copied
payload bytes, making the limit reachable beneath the wire-head ceiling. Parser/head/informational
failures SHALL take precedence before a final head exists. After a complete final head, metadata
overflow SHALL return `ProxyMetadataLimit` and allocation refusal SHALL remain `OutOfMemoryError`,
each before the status-specific error. Cleanup failure SHALL NOT replace any protected outcome. A
rejection SHALL close without unbounded body draining, retry, downgrade, or transient capability
caching.

#### Scenario: Enter a tunnel on non-200 success

- **WHEN** the proxy returns a final 204 CONNECT response with misleading Content-Length and
  Transfer-Encoding fields followed in the same read by tunnel bytes
- **THEN** the response head ends HTTP processing and the first tunneled read receives those bytes
  exactly once

#### Scenario: Own bounded authentication challenges

- **WHEN** the proxy returns 407 with challenge fields followed by an arbitrary response body
- **THEN** the route returns `ProxyAuthenticationRequired` with owned status/reason and all
  `Proxy-Authenticate` duplicates in original order under the exact copy limits, closes without
  draining the body, and performs no credential retry

#### Scenario: Prefer owned metadata capacity failure

- **WHEN** a complete non-2xx head passes parser limits but its selected owned metadata exceeds the
  32768-byte total-owned copy bound because copied field records add to its payload bytes
- **THEN** the route returns `ProxyMetadataLimit`, preserves that failure through cleanup, and does
  not publish a borrowed or partial rejection value

#### Scenario: Reject invalid transitions and head limits

- **WHEN** CONNECT receives status 101, a malformed head, more than 8 informational responses, more
  than 65536 informational head bytes, more than 100 fields, or a head beyond 32768 bytes
- **THEN** it returns the corresponding typed transition, parse, or limit failure and closes without
  tunnel publication or fallback

### Requirement: Tunnel ownership composes with original-origin TLS exactly once

Successful CONNECT SHALL expose one single-use, allocation-free tunnel-transfer operation. An
invalid or repeated transition SHALL fail while the HTTP owner remains armed. A valid transition
SHALL construct a complete affine `ByteDuplex`, then atomically disarm the HTTP owner's physical
close authority and publish the duplex, with no fallible or cancelable step between disarm and
publication. The duplex SHALL own the exact concrete transport close authority and unread buffered
suffix. Reads SHALL serve the suffix before underlying transport input; writes and flushes SHALL
forward unchanged. Its complete close SHALL be terminal and idempotent and SHALL attempt the
concrete close at most once.

The transferred duplex SHALL retain the concrete provider's write-direction authority:
`shutdownWrite` SHALL perform the canonical flush and forward directional shutdown exactly once
without closing the read direction. The duplex SHALL remain live for terminal close. The HTTP
exchange SHALL become permanently unusable and the tunnel SHALL NOT return to a forward-proxy pool.

For an HTTPS origin, TLS authentication SHALL begin only after CONNECT succeeds, consume a prepared
owned trust snapshot, and verify the original origin host and security context rather than the proxy
host or a resolved alias. HTTP inside the authenticated tunnel SHALL use origin-form targets and
SHALL NOT emit `Proxy-Authorization`. No nested encrypted proxy layer SHALL be assumed.

The scoped route API SHALL lend only the resulting exclusive `HttpTransport` to the client's
connected operation. It SHALL preserve the callback's success, failure, and requirement channels;
prevent the callback from escaping or duplicating the transport borrow; and close the physical
owner at most once after success, typed failure, structured cancellation, TLS failure, or callback
failure without replacing the protected outcome.

The target-neutral acquisition adapter supplied to `withRoute` SHALL lend one owned plain provider
that implements both `HttpTransport` and `ByteDuplex` for exactly the selected physical peer and
unchanged deadline. It SHALL also carry explicit HTTP limits, HTTP version/ALPN policy, TLS client
limits, and finite handshake duration. `withRoute` SHALL use those values for the resulting
connection and origin TLS; it SHALL NOT silently substitute defaults. The native
`withProxyRoute(route, options, limits, preparedTrust, handler)` sibling SHALL derive the adapter
values exactly from its `options` and `limits` arguments.

#### Scenario: Preserve explicit client settings

- **WHEN** a route client supplies nondefault HTTP limits, version/ALPN policy, TLS limits, and
  handshake duration
- **THEN** routing and tunneled authentication use those exact settings without replacing any with
  library defaults

#### Scenario: Authenticate only after CONNECT

- **WHEN** a scripted HTTPS proxy route completes CONNECT and then supplies a certificate for the
  original origin
- **THEN** TLS emits no byte before the 2xx transition, authenticates the original origin, and lends
  a secured HTTP transport only after authentication completes

#### Scenario: Reject the proxy identity as the origin

- **WHEN** CONNECT succeeds but the tunneled TLS peer is valid only for the proxy hostname and not
  the requested origin
- **THEN** origin authentication fails, no HTTP request is sent through the tunnel, and the owner
  closes once

#### Scenario: Preserve callback failure during cleanup

- **WHEN** the route callback returns its own typed failure and terminal transport close also fails
- **THEN** cleanup attempts physical close at most once and the callback failure remains observable

#### Scenario: Keep the outer owner armed until publication

- **WHEN** tunnel transfer is requested in an invalid state or a repeated transfer is attempted
- **THEN** no duplex is published and the existing HTTP owner retains its close obligation

#### Scenario: Preserve directional shutdown across transfer

- **WHEN** the transferred tunnel duplex receives `shutdownWrite`
- **THEN** it flushes, forwards the concrete provider's directional shutdown exactly once, preserves
  the read direction, and remains available for one terminal close

### Requirement: One absolute deadline spans every route phase

The caller SHALL establish the route's optional absolute deadline before loading one owned
`TrustSnapshot`, finish that load before entering the route scope, and pass the same unchanged
absolute mark with the snapshot. The route API SHALL consume that prepared snapshot for a secure
Direct or Tunnel route and SHALL require no snapshot for a Forward or insecure Direct route;
invalid presence/absence SHALL fail before acquisition. This feature does not make trust loading
interruptible, but elapsed loading time counts against a finite absolute deadline because no fresh
mark is created at route entry.

The route API SHALL accept that one optional absolute monotonic deadline and pass it unchanged
through proxy route acquisition, CONNECT, tunnel TLS authentication, and the connected HTTP scope.
TLS SHALL clamp its finite handshake duration against that external deadline without restarting
elapsed time. Native synchronous hostname resolution with an overall deadline SHALL return its
existing unsupported-deadline failure before dispatch; a numeric proxy route SHALL retain and use
the absolute deadline.

#### Scenario: Do not restart after CONNECT

- **WHEN** CONNECT consumes part of a finite route deadline after trust was prepared by the caller
- **THEN** tunneled TLS receives the original absolute deadline and times out at that boundary rather
  than receiving a fresh duration

#### Scenario: Count caller-owned trust preparation without claiming interruption

- **WHEN** the caller establishes a finite absolute deadline, loads trust, and then enters a secure
  route with the resulting owned snapshot and that same mark
- **THEN** route acquisition consumes that snapshot, observes the elapsed loading time through the
  unchanged mark, and makes no claim that the preceding trust load was deadline-interruptible

#### Scenario: Reject a synchronous hostname deadline before dispatch

- **WHEN** a native route has a proxy domain name and a finite overall deadline
- **THEN** route acquisition returns the resolver's unsupported-deadline failure before native DNS
  work or connection output

### Requirement: Errors and route hooks remain bounded and composable

Proxy failures SHALL distinguish invalid configuration, configuration-time unsupported proxy
transport, authentication required, rejected CONNECT status, owned-metadata limits,
informational/head limits, and malformed proxy responses while preserving existing resolver,
connection, parser, byte-I/O, allocation, TLS, identity, and trust failures. Proxy-owned status,
headers, and challenges SHALL obey the exact copy limits above and SHALL NOT borrow from a closed
response head or retain credentials or identity payloads in diagnostics.

The capability SHALL expose stable route-key data containing proxy endpoint identity, exact opaque
caller-supplied configuration/authentication identities, Forward versus Tunnel mode, original
origin, and origin security context for later pooling. Copying a route/key SHALL preserve those IDs;
equality SHALL compare them exactly without formatting or hashing credentials. It SHALL also expose
pure route recomputation from a new origin under the exact immutable configuration retained by the
route, so redirect policy can reapply bypass and credential selection without silently switching
configuration or credentials. Explicit configuration replacement SHALL call `selectRoute` with the
new configuration. These hooks SHALL NOT implement pool storage, redirect following, retry, replay,
or permanent proxy-capability caching.

#### Scenario: Distinguish a proxy rejection from transport failure

- **WHEN** a complete bounded non-2xx CONNECT head is received
- **THEN** the caller receives the proxy status failure with owned metadata rather than a parser,
  transport, or generic invalid-state error

#### Scenario: Recompute without carrying authority credentials

- **WHEN** a downstream redirect policy supplies a different origin to route recomputation
- **THEN** the route's exact retained configuration reapplies bypass and proxy authentication
  selection and derives mode and route identity anew without carrying origin credentials or an
  established tunnel flag from the previous origin

### Requirement: Proxy delivery is portable, generated, and bounded

The standard-library manifest, generated source catalog, generated API reference, prescriptive
reference, and examples SHALL expose the proxy actors, supported route matrix, plaintext credential
risk, limits, ownership, errors, deadlines, and exclusions. Scripted transports SHALL execute on
the shared native and intended LLVM-to-Wasm engines. Physical native acquisition SHALL inherit the
existing admitted socket targets and SHALL return a documented unsupported result elsewhere.

Default verification SHALL use exactly one shared `Analysis` snapshot for configuration, selection,
routed preparation, and diagnostics; one compact shared runtime corpus program for distinct Forward,
CONNECT, suffix, and close/callback signals; one already existing TLS fixture vector for the unique
CONNECT-to-origin-TLS boundary; and structural native preflight assertions for endpoint choice and
deadline refusal. Exactly one LLVM-to-Wasm leg SHALL prove the named ordinary-source
routed-preparation-and-scripted-tunnel portability claim. Verification SHALL add no stress or timing
suite, per-case compilation, fresh-process matrix, duplicate TLS matrix, or new standalone test
worker.

#### Scenario: Import the portable proxy policy

- **WHEN** a supported program imports `silk.http_proxy` without using physical native acquisition
- **THEN** configuration, route selection, request preparation, and scripted tunnel composition are
  available as ordinary source on native and intended LLVM-to-Wasm execution

#### Scenario: Report unsupported physical acquisition

- **WHEN** a program requests native proxy routing on a target outside the admitted native socket
  profiles
- **THEN** it receives the documented unsupported result without implying socket or TLS support
