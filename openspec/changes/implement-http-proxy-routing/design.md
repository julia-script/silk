## Context

See `proposal.md` for motivation. The delivered client already owns bounded head parsing,
absolute-deadline exchange state, CONNECT recognition, and exclusive suffix-preserving tunnel
handoff. Native sockets own physical connections, while TLS authentication can consume an owned
byte duplex and verify an origin identity under an external absolute deadline. Direct request
preparation intentionally rejects `Proxy-Authorization`.

The proxy feature therefore composes existing actors. It must introduce route/configuration policy
and the smallest ownership bridge needed to move a successful CONNECT tunnel into TLS, without a
second HTTP parser, TLS pump, resolver, or compiler-known operation.

## Goals / Non-Goals

**Goals:**

- Keep proxy configuration, authentication identity, route selection, and wire preparation in one
  ordinary-source actor with bounded owned state.
- Make Forward and Tunnel routes explicit, preserving original-origin identity and one absolute
  deadline across acquisition, CONNECT, trust loading, TLS, and the HTTP callback.
- Transfer the successful CONNECT suffix and physical close authority exactly once.
- Publish stable pure route-key and recomputation inputs for later pooling and redirects.

**Non-Goals:**

- Environment/PAC discovery, TLS-encrypted proxy endpoints, SOCKS, proxy chains, reverse proxying,
  automatic 407 retry, direct-route fallback, pooling, redirects, or a new TLS implementation.
- Generalizing the HTTP client around proxy policy or making proxy credentials origin headers.

## Decisions

### One actor owns all proxy policy

Add `silk.http_proxy` as the canonical owner of `ProxyConfig`, `ProxyAuth`, `BypassPolicy`, `Route`,
route identity, request preparation, and proxy-specific failures. Keeping these concepts together
makes it impossible for the generic request path to acquire proxy credentials accidentally.

Alternative: place absolute-form and CONNECT helpers in `http_request`. Rejected because that actor
is origin-bound and deliberately rejects `Proxy-Authorization`; weakening it would create two
credential authorities.

### Route selection is pure; route use is scoped

`selectRoute` validates only immutable configuration/origin relationships and returns Direct,
Forward, or Tunnel data. `withRoute` performs acquisition and lends the resulting transport through
a higher-ranked handler, preserving caller channels. Bypass is decided before auth header material
is copied into request scratch.

Alternative: let acquisition implicitly fall back to direct. Rejected because a proxy failure must
not silently change the security route.

### Reuse the existing CONNECT state machine

Build CONNECT as a body-free prepared request and run it through the current client exchange. The
existing `withTunnel` operation is the only transition from HTTP parsing to tunneled bytes. Extend
its tunnel view only with the minimal `ByteDuplex`-compatible read/write/flush/close adapter needed
by `tls_connection.authenticateOwned`.

The outer HTTP connection is marked physically transferred before the tunnel owner can be consumed.
After transfer, only the tunnel/TLS owner closes the concrete provider. Cleanup remains armed until
that transfer completes, so failure before transfer still closes through the HTTP owner.

Alternative: parse CONNECT separately in the proxy actor. Rejected because it would duplicate head
limits, informational responses, suffix handling, and terminal state rules.

### Forward and tunneled request preparation stay distinct

Forward mode constructs an absolute-form target and injects the configured proxy credential while
retaining the origin `Host`. Tunnel setup constructs authority-form CONNECT with only proxy-hop
metadata. After TLS authentication, ordinary origin-form preparation is reused and contains no
proxy credential.

### Existing bounded primitives define capacity and identity

Use `http_origin`/`network_address` equality for exact bypass and route identity, `uri` and
`http_target` for lossless target construction, `base64` strict validation for prepared Basic
tokens, and checked `usize` arithmetic before allocation. Authentication-context identity is an
opaque monotonic/configuration value that never hashes or formats secret bytes.

### Verification shares expensive boundaries

Pure configuration, target, and selection claims use structured/static analysis. A compact support
program supplies scripted proxy transitions to the existing shared native and intended-Wasm corpus.
It reuses head/TLS fixtures rather than repeating their matrices and adds only boundary cases unique
to routing, credential separation, suffix transfer, and close ownership.

## Risks / Trade-offs

- [Tunnel transfer can double-close or leak the provider] -> represent transfer as a single affine
  state transition, disarm exactly one owner only after successful move, and exercise every failure
  boundary with release counters.
- [Generic callback/provider rows can expose a compiler limitation] -> follow the delivered client
  and TLS higher-ranked handler patterns and prove ownership with structured analysis before adding
  runtime cases.
- [Forward request construction can leak proxy credentials into a tunnel] -> use separate private
  builders and reject caller `Proxy-Authorization` before any bytes are serialized.
- [Large proxy fixtures can duplicate costly parser/TLS coverage] -> keep one composed corpus path
  and import existing fixture vectors for the lower-level behavior.
- [Plain proxy authentication is observable on the network] -> make the route matrix and reference
  documentation explicit; the API never labels the proxy hop protected.

## Migration Plan

This is a new green-field actor. Add and register the module, then update all in-repository examples
to the final API in the same change. Rollback is deletion of the new actor, registrations, tests,
and docs; no persisted format or compatibility path exists.
