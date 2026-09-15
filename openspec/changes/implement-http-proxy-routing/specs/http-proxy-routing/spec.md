## Purpose

Define explicit, bounded HTTP forward-proxy routing that keeps proxy identity and credentials
separate from the origin while safely composing CONNECT tunnels with origin-authenticated TLS.

## ADDED Requirements

### Requirement: Proxy configuration is explicit, immutable, and bounded

Canonical ordinary-source `silk.http_proxy` SHALL expose owned `ProxyConfig`, `ProxyAuth`, and
`BypassPolicy` values. Configuration construction SHALL copy and validate a proxy host and a port
from 1 through 65535, at most 64 exact bypass origins, at most 4096 encoded authentication-token
bytes, and an explicit `maxOwnedConfigBytes` covering copied hosts, credentials, entries, and index
capacities. All size arithmetic SHALL be checked, and invalid syntax or insufficient capacity SHALL
fail before resolution, connection, authentication generation, or wire output. Configuration SHALL
NOT read environment variables, PAC files, system settings, or another ambient credential source.

An optional URI convenience constructor SHALL admit only a plain `http` authority with no
userinfo, path beyond empty, query, or fragment. SOCKS, proxy chains, reverse proxies, and
TLS-encrypted proxy endpoints SHALL remain unrepresented as successful configurations.

#### Scenario: Reject capacity before contact

- **WHEN** copied proxy configuration exceeds an entry, credential, or owned-byte limit or overflows
  checked size arithmetic
- **THEN** construction returns a typed configuration failure without DNS, connection, trust,
  authentication generation, or transport output

#### Scenario: Parse only a plain proxy authority

- **WHEN** the convenience constructor receives an `https` URI or a URI containing userinfo, path,
  query, fragment, an empty host, or a port outside 1 through 65535
- **THEN** it rejects the configuration rather than stripping components or selecting another route

### Requirement: Prepared proxy authentication is canonical and secret-safe

`ProxyAuth` SHALL be either `None` or an explicitly supplied nonempty prepared Basic token. A
prepared token SHALL use the canonical strict standard-padded Base64 validator and SHALL reject
noncanonical padding, an invalid alphabet, whitespace, controls, and nonzero pad bits before
contact. Validation SHALL use no more than 3072 bytes of bounded scratch for the maximum admitted
token and SHALL NOT reinterpret username or password character sets. Digest, NTLM, Negotiate,
ambient credential discovery, and automatic 407 retry SHALL NOT be provided.

The configuration SHALL publish an opaque authentication-context identity suitable for route keys.
Neither that identity nor typed diagnostics SHALL print, retain in diagnostic metadata, or derive a
public hash from secret token bytes. Bypass selection SHALL occur before proxy credentials or a
`Proxy-Authorization` field are produced.

#### Scenario: Reject a malformed prepared token

- **WHEN** a nonempty prepared Basic token contains URL-safe bytes, whitespace, controls, malformed
  padding, nonzero unused pad bits, or exceeds the encoded-token limit
- **THEN** configuration fails before any proxy contact and exposes no secret bytes in the error

#### Scenario: Bypass without touching credentials

- **WHEN** an origin matches an exact bypass entry
- **THEN** route selection returns a direct route without producing proxy authentication bytes

### Requirement: Route selection implements the admitted matrix without fallback

`selectRoute(config, origin)` SHALL be pure and SHALL return a direct route for an exact bypass
match, a Forward route for an HTTP origin through a plain HTTP proxy, and a Tunnel route for an
HTTPS origin through a plain HTTP proxy. Exact bypass comparison SHALL include scheme, normalized
host identity, and effective port; it SHALL use equivalent canonical numeric-address forms and
SHALL NOT perform suffix, wildcard, CIDR, or DNS-result matching. Equal proxy and origin host text
SHALL NOT implicitly bypass an explicitly configured proxy.

A successful route SHALL retain immutable proxy host and port, opaque configuration and
authentication-context identity, original origin, Forward or Tunnel mode, and origin security
context. A requested TLS-encrypted proxy route SHALL return `UnsupportedProxyTransport` before DNS
or connection acquisition. A proxy failure SHALL never select a direct route, plaintext origin
connection, or retry automatically.

#### Scenario: Select every admitted route

- **WHEN** route selection receives an HTTP origin, an HTTPS origin, and an exact bypass origin
  under one plain proxy configuration
- **THEN** it returns respectively Forward, Tunnel, and Direct routes with the original origin and
  immutable route identity intact

#### Scenario: Refuse an encrypted proxy before acquisition

- **WHEN** a route describes an HTTP or HTTPS origin through a TLS-encrypted proxy endpoint
- **THEN** acquisition returns `UnsupportedProxyTransport` before resolver or connection dispatch
  and does not fall back to a direct or plaintext origin route

### Requirement: Forward routes send one origin-bound absolute-form request

An HTTP Forward route SHALL connect to the proxy endpoint and serialize the original URI as an
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
SHALL ignore `Content-Length` and `Transfer-Encoding` on that successful response. A 407 response
SHALL return `ProxyAuthenticationRequired` with bounded owned challenge metadata. Other non-2xx
responses SHALL return `ProxyRejected` with bounded owned status and header metadata. A rejection
SHALL close without unbounded body draining, retry, downgrade, or transient capability caching.

#### Scenario: Enter a tunnel on non-200 success

- **WHEN** the proxy returns a final 204 CONNECT response with misleading Content-Length and
  Transfer-Encoding fields followed in the same read by tunnel bytes
- **THEN** the response head ends HTTP processing and the first tunneled read receives those bytes
  exactly once

#### Scenario: Own bounded authentication challenges

- **WHEN** the proxy returns 407 with challenge fields followed by an arbitrary response body
- **THEN** the route returns `ProxyAuthenticationRequired` with bounded owned status/challenge
  metadata, closes without draining the body, and performs no credential retry

#### Scenario: Reject invalid transitions and head limits

- **WHEN** CONNECT receives status 101, a malformed head, more than 8 informational responses, more
  than 65536 informational head bytes, more than 100 fields, or a head beyond 32768 bytes
- **THEN** it returns the corresponding typed transition, parse, or limit failure and closes without
  tunnel publication or fallback

### Requirement: Tunnel ownership composes with original-origin TLS exactly once

Successful CONNECT SHALL transfer the exact concrete transport authority and unread buffered suffix
into one scoped byte-duplex tunnel. Reads SHALL serve the suffix before underlying transport input;
writes and flushes SHALL forward unchanged. The HTTP exchange SHALL become permanently unusable and
the tunnel SHALL NOT return to a forward-proxy pool.

For an HTTPS origin, TLS authentication SHALL begin only after CONNECT succeeds, consume a prepared
owned trust snapshot, and verify the original origin host and security context rather than the proxy
host or a resolved alias. HTTP inside the authenticated tunnel SHALL use origin-form targets and
SHALL NOT emit `Proxy-Authorization`. No nested encrypted proxy layer SHALL be assumed.

The scoped route API SHALL lend only the resulting exclusive `HttpTransport` to the client's
connected operation. It SHALL preserve the callback's success, failure, and requirement channels;
prevent the callback from escaping or duplicating the transport borrow; and close the physical
owner at most once after success, typed failure, structured cancellation, TLS failure, or callback
failure without replacing the protected outcome.

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

### Requirement: One absolute deadline spans every route phase

The route API SHALL accept one optional absolute monotonic deadline and pass it unchanged through
proxy route acquisition, CONNECT, caller-owned trust preparation, tunnel TLS authentication, and
the connected HTTP scope. TLS SHALL clamp its finite handshake duration against that external
deadline without restarting elapsed time. Native synchronous hostname resolution with an overall
deadline SHALL return its existing unsupported-deadline failure before dispatch; a numeric proxy
route SHALL retain and use the absolute deadline.

#### Scenario: Do not restart after CONNECT

- **WHEN** CONNECT and trust loading consume part of a finite route deadline
- **THEN** tunneled TLS receives the original absolute deadline and times out at that boundary rather
  than receiving a fresh duration

#### Scenario: Reject a synchronous hostname deadline before dispatch

- **WHEN** a native route has a proxy domain name and a finite overall deadline
- **THEN** route acquisition returns the resolver's unsupported-deadline failure before native DNS
  work or connection output

### Requirement: Errors and route hooks remain bounded and composable

Proxy failures SHALL distinguish invalid configuration, unsupported proxy transport,
authentication required, rejected CONNECT status, informational/head limits, and malformed proxy
responses while preserving existing resolver, connection, parser, byte-I/O, allocation, TLS,
identity, and trust failures. Proxy-owned status, headers, and challenges SHALL remain within stated
limits and SHALL NOT borrow from a closed response head or retain credentials in diagnostics.

The capability SHALL expose stable route-key data containing proxy endpoint identity, opaque
configuration/authentication identity, Forward versus Tunnel mode, original origin, and origin
security context for later pooling. It SHALL also expose pure route recomputation from a new origin
so redirect policy can reapply bypass and credential selection. These hooks SHALL NOT implement
pool storage, redirect following, retry, replay, or permanent proxy-capability caching.

#### Scenario: Distinguish a proxy rejection from transport failure

- **WHEN** a complete bounded non-2xx CONNECT head is received
- **THEN** the caller receives the proxy status failure with owned metadata rather than a parser,
  transport, or generic invalid-state error

#### Scenario: Recompute without carrying authority credentials

- **WHEN** a downstream redirect policy supplies a different origin to route recomputation
- **THEN** bypass, proxy authentication selection, mode, and route identity are derived anew without
  carrying origin credentials or an established tunnel flag from the previous origin

### Requirement: Proxy delivery is portable, generated, and bounded

The standard-library manifest, generated source catalog, generated API reference, prescriptive
reference, and examples SHALL expose the proxy actors, supported route matrix, plaintext credential
risk, limits, ownership, errors, deadlines, and exclusions. Scripted transports SHALL execute on
the shared native and intended LLVM-to-Wasm engines. Physical native acquisition SHALL inherit the
existing admitted socket targets and SHALL return a documented unsupported result elsewhere.

Default verification SHALL reuse shared HTTP head and client fixtures and the existing native
acceptance corpus. It SHALL use a small distinct composed-transition corpus rather than duplicate
parser, TLS, or per-feature fresh-process suites, and SHALL include independent correctness and
test-economics review.

#### Scenario: Import the portable proxy policy

- **WHEN** a supported program imports `silk.http_proxy` without using physical native acquisition
- **THEN** configuration, route selection, request preparation, and scripted tunnel composition are
  available as ordinary source on native and intended LLVM-to-Wasm execution

#### Scenario: Report unsupported physical acquisition

- **WHEN** a program requests native proxy routing on a target outside the admitted native socket
  profiles
- **THEN** it receives the documented unsupported result without implying socket or TLS support
