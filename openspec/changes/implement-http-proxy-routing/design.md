## Context

See `proposal.md` for motivation. The delivered client already owns bounded head parsing,
absolute-deadline exchange state, CONNECT recognition, and exclusive suffix-preserving tunnel
handoff. Native sockets own physical connections, while TLS authentication can consume an owned
byte duplex and verify an origin identity under an external absolute deadline. Direct request
preparation intentionally rejects `Proxy-Authorization`.

The proxy feature therefore composes existing actors. It introduces route/configuration policy, a
narrow routed-request admission boundary, and the smallest ownership bridge needed to move a
successful CONNECT tunnel into TLS, without a second HTTP parser, TLS pump, resolver, or
compiler-known operation.

## Goals / Non-Goals

**Goals:**

- Keep proxy configuration, caller-supplied nonsecret identities, route selection, and proxy wire
  policy in one ordinary-source actor with bounded owned state.
- Make Forward and Tunnel routes explicit, preserving original-origin identity and one absolute
  deadline across caller-owned trust preparation, acquisition, CONNECT, TLS, and the HTTP callback.
  Trust loading completes before route entry and is not made interruptible by this feature, but
  elapsed loading time counts when the caller established the same absolute deadline first.
- Transfer the successful CONNECT suffix and physical close authority exactly once.
- Publish stable pure route-key and recomputation inputs for later pooling and redirects.

**Non-Goals:**

- Environment/PAC discovery, TLS-encrypted proxy endpoints, SOCKS, proxy chains, reverse proxying,
  automatic 407 retry, direct-route fallback, pooling, redirects, or a new TLS implementation.
- Generalizing the HTTP client around proxy policy or making proxy credentials origin headers.

## Decisions

### One actor owns all proxy policy

Add `silk.http_proxy` as the canonical owner of `ProxyConfig`, `ProxyAuth`, `BypassPolicy`, `Route`,
route identity, proxy request policy, and proxy-specific failures. Configuration construction takes
opaque `ProxyConfigId` and `ProxyAuthContextId` values made from caller-supplied nonsecret `u64`
values. Both IDs are `Copy`, compare by exact value, and survive configuration/route copies
unchanged. The caller must choose a new authentication-context ID whenever the credential or its
authorization context changes; Silk never derives either ID from secret bytes.

`http_request` remains the lower-level serializer. It gains one explicitly routed admission
operation whose inputs include the logical origin, physical peer origin, Forward-versus-CONNECT
mode, and proxy-policy-owned prepared credential. Its resulting `PreparedRequest` retains both
origins: serialization and origin `Authorization`/`Host` validation use the logical origin, while
`http_client.withExchange` admits it only on a connection for the physical peer. The existing
direct `prepare` operation sets both origins equal and continues to reject caller
`Proxy-Authorization`. Thus the generic direct path cannot acquire a proxy credential accidentally,
and a routed request cannot be replayed on the authenticated origin connection inside a tunnel.

Alternative: serialize a second private request-head representation in `http_proxy`. Rejected
because `PreparedRequest` is sealed and duplicating its admission/framing invariants would create a
second HTTP request authority.

### Route selection is pure; route use is scoped

`selectRoute` validates only immutable configuration/origin relationships and returns Direct,
Forward, or Tunnel data. Bypass is decided before auth header material is copied into request
scratch.

The target-neutral scoped shape is
`withRoute(client, route, preparedTrust, deadline, handler)`. `client` is a scoped peer-acquisition
adapter that accepts exactly one selected endpoint and the unchanged deadline and lends its owned
plain transport with complete `HttpTransport` and `ByteDuplex` authority; it does not select a route
or perform TLS. The adapter also carries the explicit HTTP limits, HTTP version/ALPN policy, TLS
client limits, and finite handshake duration needed after acquisition. These values are never
silently defaulted inside `withRoute`. `preparedTrust` is `None` for a Forward route or an insecure
Direct route and is one caller-prepared owned `TrustSnapshot` for a secure Direct or Tunnel route.
Invalid presence/absence fails before acquisition. The operation consumes `route` and
`preparedTrust`, and invokes one higher-ranked handler with the existing callback-scoped HTTP
connection; the connection cannot escape or be duplicated.

For arbitrary handler success `A`, failure `E`, and requirements `R`, `withRoute` returns `A`, fails
with exactly `E | ProxyError | ClientError | ConnectionError | IdentityError | OutOfMemoryError`
plus the acquisition adapter's declared failures, and requires exactly `R` plus the adapter's
requirements and the existing allocator/monotonic-clock/system-clock/random requirements. It never
erases or wraps `E` or `R`. `http_client_native.withProxyRoute(route, options, limits,
preparedTrust, handler)` is the concrete native sibling: it has the same callback channels, adds the
existing resolver/socket/native errors and requirements, and resolves/connects only `route`'s
selected physical endpoint. `Options.deadline` is the one route deadline; no second deadline
parameter is accepted.

Alternative: let acquisition implicitly fall back to direct. Rejected because a proxy failure must
not silently change the security route.

### Reuse the existing CONNECT state machine

Build CONNECT as a body-free routed prepared request and run it through the current client exchange.
The existing `withTunnel` operation is the only transition from HTTP parsing to tunneled bytes. Add
one single-use `Tunnel.transferByteDuplex` transition. It is allocation-free and infallible after
its state precondition: an invalid or repeated call fails while the HTTP owner remains armed; a
valid call first constructs the complete adapter, then atomically marks the HTTP connection
physically transferred and returns the affine adapter. There is no fallible or cancelable operation
between disarming the HTTP owner and publishing the adapter.

The adapter serves the retained suffix before transport input and forwards read, write, flush, and
write-direction shutdown to the concrete `ByteDuplex` provider under the tunnel's clamped deadline.
Its idempotent complete close calls the concrete close once and marks itself terminal. The HTTP
transport abstraction's lack of half-close does not erase the underlying provider's
`ByteDuplex.shutdownWriteRaw` authority during the affine transfer. After publication only the
adapter/TLS owner has close authority; the outer HTTP release observes the transfer mark and
performs no transport close. TLS authentication failure and callback cleanup close through the
same adapter owner.

Alternative: parse CONNECT separately in the proxy actor. Rejected because it would duplicate head
limits, informational responses, suffix handling, and terminal state rules.

### Forward and tunneled request preparation stay distinct

Forward mode constructs an absolute-form target and injects the configured proxy credential while
retaining the origin `Host`. Tunnel setup constructs authority-form CONNECT with only proxy-hop
metadata. After TLS authentication, ordinary origin-form preparation is reused and contains no
proxy credential.

### Encrypted proxies fail during configuration

The programmatic constructor accepts only a plain proxy endpoint, and the URI convenience
constructor returns `UnsupportedProxyTransport` for `https` before producing `ProxyConfig`.
Consequently route selection and acquisition have no encrypted-proxy variant and cannot contain an
unreachable second rejection path.

### CONNECT rejection metadata has one exact owned shape

Both rejection errors own the final status code and reason bytes. `ProxyAuthenticationRequired`
retains only every `Proxy-Authenticate` field in original order, including duplicates;
`ProxyRejected` retains every response field in original order. Copying uses value limits of 100
fields, 256 name bytes, 8192 value bytes, 32768 aggregate field bytes, and 32768 total owned bytes.
The total-owned bound intentionally includes field-record storage in addition to copied payload, so
it remains reachable beneath the 32768-byte wire-head ceiling rather than specifying a dead error
branch. Parser, head, and informational limits take precedence before a final head exists. For a
complete final head, metadata overflow becomes `ProxyMetadataLimit` and allocation refusal remains
`OutOfMemoryError`; either precedes the status-specific error, and cleanup failure never replaces
it.

### Existing bounded primitives define capacity and identity

Use `http_origin`/`network_address` equality for exact bypass and route identity, `uri` and
`http_target` for lossless target construction, `base64` strict validation for prepared Basic
tokens, and checked `usize` arithmetic before allocation. Route keys copy and compare the exact
caller-supplied configuration/authentication IDs; their formatting and diagnostics expose neither
the numeric ID payload nor credential bytes.

### Verification shares expensive boundaries

All configuration, selection, routed-preparation, and diagnostic claims share one `Analysis`
snapshot. One compact runtime corpus program carries the distinct Forward, CONNECT, suffix, and
close/callback signals. One existing TLS fixture vector is reused for the sole CONNECT-to-origin-TLS
boundary case. Native endpoint choice and deadline refusal use structural preflight assertions, not
extra physical executions. Exactly one LLVM-to-Wasm leg proves the named ordinary-source
routed-preparation-and-scripted-tunnel portability claim. No stress, timing, per-case compilation,
fresh-process matrix, duplicate TLS matrix, or new test worker is added.

## Risks / Trade-offs

- [Tunnel transfer can double-close or leak the provider] -> use the allocation-free single affine
  transition above, with the outer owner armed until publication and one compact release-counter
  runtime signal covering success and protected callback failure.
- [Generic callback/provider rows can expose a compiler limitation] -> follow the delivered client
  and TLS higher-ranked handler patterns and prove ownership with structured analysis before adding
  runtime cases.
- [Forward request construction can leak proxy credentials into a tunnel] -> use separate private
  builders and reject caller `Proxy-Authorization` before any bytes are serialized.
- [Large proxy fixtures can duplicate costly parser/TLS coverage or exhaust compiler memory] ->
  enforce the fixed snapshot/corpus/vector/preflight/Wasm evidence budget above and add no stress,
  timing, per-case, fresh-process, duplicate-matrix, or standalone-worker coverage.
- [Plain proxy authentication is observable on the network] -> make the route matrix and reference
  documentation explicit; the API never labels the proxy hop protected.

## Migration Plan

This is a new green-field actor. Add and register the module, then update all in-repository examples
to the final API in the same change. Rollback is deletion of the new actor, registrations, tests,
and docs; no persisted format or compatibility path exists.
