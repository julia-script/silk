## Why

Silk's streaming HTTP client can connect directly and hand a successful CONNECT response to a
scoped tunnel, but it cannot select an explicit proxy route or compose that tunnel with
origin-authenticated TLS. Applications otherwise have to reproduce credential separation,
request-target selection, buffered-suffix ownership, and deadline propagation, where a mistake can
leak proxy credentials, authenticate the wrong peer, discard tunneled bytes, or silently downgrade
to a direct route.

## What Changes

- Add bounded, immutable proxy configuration, prepared Basic proxy authentication, exact-origin
  bypass policy, pure route selection, and caller-supplied nonsecret configuration/authentication
  identities with exact copy and equality semantics.
- Add explicit plain-proxy routing: absolute-form forwarding for HTTP origins and body-free CONNECT
  followed by TLS authenticated for the original HTTPS origin.
- Reject encrypted proxy transports before resolution or connection, and never retry, downgrade, or
  fall back to a direct route after a proxy failure.
- Add a narrow routed-request admission path that binds a logical request origin separately from its
  physical peer while leaving direct preparation's proxy-credential rejection intact.
- Preserve the exact unread CONNECT suffix through one single-use complete `ByteDuplex`, the
  caller's unchanged absolute deadline, typed failure channels, and close-once transport ownership.
- Publish route identity and recomputation inputs for later pooling and redirect policies without
  implementing either policy.
- Register and document the ordinary-source API and add bounded scripted/native/Wasm acceptance
  evidence that reuses the existing HTTP head, client, socket, TLS, and Base64 surfaces.

## Capabilities

### New Capabilities

- `http-proxy-routing`: Explicit bounded forward-proxy configuration, route selection, proxy-only
  credentials, HTTP absolute-form forwarding, HTTPS CONNECT-to-origin-TLS transition, route keys,
  ownership, deadlines, failures, and delivery evidence.

### Modified Capabilities

None. The routed-admission and successful-CONNECT transfer requirements are specified as part of
the new proxy-routing capability because the repository does not yet have an archived main
`streaming-http-client` capability to amend. The implementation still extends the existing
`http_request` and `http_client` actors, and direct request preparation continues to reject caller
`Proxy-Authorization`.

## Impact

The change adds canonical `silk.http_proxy` source and generated manifest/documentation surfaces.
It extends `silk.http_request`/`silk.http_client` with the routed-admission and tunnel-transfer
contracts above and adds a native proxy acquisition entry point beside `silk.http_client_native`,
while preserving direct-client behavior. Compiler evidence uses one shared analysis snapshot, one
compact runtime corpus, one reused TLS vector, structural native preflight, and exactly one named
LLVM-to-Wasm portability leg. No compiler intrinsic, environment/PAC discovery, SOCKS, proxy chain,
reverse proxy, TLS-encrypted proxy, automatic 407 retry, redirect, pooling, fetch policy, stress
matrix, or fresh-process matrix is introduced. Tracking: https://linear.app/juliaortiz/issue/JUL-199.
