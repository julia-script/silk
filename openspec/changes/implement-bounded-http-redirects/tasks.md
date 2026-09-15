## 1. Redirect values and replay contracts

- [ ] 1.1 Add `silk.http_redirect` policy, request, status-policy, previous-response, bounded name-list, history-limit, error, and owned-context values with checked pre-I/O admission; encode defaults, overlap rejection, arithmetic overflow, and exact capacity boundaries in the shared HTTP analysis/static-evaluation source.
- [ ] 1.2 Add generic `BodyProducer`, scoped replay-factory, and `BodySource` Empty/RepeatableBytes/OneShot/ReplayFactory contracts with precise error and requirement rows; encode affine escape/duplication rejection and factory/producer lifetime constraints in that same analysis snapshot.
- [ ] 1.3 Implement the shared body writer over existing exchange operations with exact framing, declared-length, observed-length, early-final, and dropped-body behavior; add factory-attempt/release and OneShot replay signals to the single redirect runtime corpus source.

## 2. Pure redirect transitions

- [ ] 2.1 Implement the 301/302/303/307/308 method/body matrix and Manual/non-selected status behavior; encode all five statuses, HEAD, extension methods, 304, zero-hop Follow, POST-to-GET, and retained-body replay decisions in the shared analysis/static-evaluation source.
- [ ] 2.2 Implement bounded Location extraction, URI-reference parsing/resolution, RFC 9110 fragment inheritance, HTTP/HTTPS origin admission, and method-aware history keys; encode empty/missing/duplicate Location, relative/query/fragment references, equivalent host/default-port identity, loops, and raw/resolved/history boundary cases without per-case compilation.
- [ ] 2.3 Implement deny-last header rebuilding for hop-by-hop, Connection-nominated, dropped-content, cross-origin, and configured sensitive fields; encode changed port/subdomain/downgrade decisions and exact sanitized emitted bytes proving Authorization, Cookie, Proxy-Authorization, Origin, and Referer cannot survive an override.

## 3. Scoped client composition

- [x] 3.1 Add the smallest `http_client` bounded-discard outcome that distinguishes Completed from CapReached while preserving malformed framing, read, deadline, allocation, and transport failures; encode wire-overhead cap versus malformed-framing behavior in the redirect corpus source.
- [ ] 3.2 Add the higher-ranked target-neutral attempt-client/handler boundary that rebuilds each prepared request, drives one scoped exchange, and preserves generic callback/acquisition/source channels; encode nonescaping response/URI borrows and exact callback rows in the shared analysis snapshot.
- [ ] 3.3 Implement proxy-aware attempt preparation and acquisition using `Route.recompute`, Forward absolute-form preparation, Direct/Tunnel origin-form preparation, original-origin TLS, and the unchanged deadline; encode proxy-to-bypass-to-proxy route signals and assert no disallowed contact or credential leakage without adding a TLS/socket matrix.
- [ ] 3.4 Implement iterative `Redirect.withResponse` state ownership, final callback invocation, intermediate Close/Drain handling, and nested producer/attempt finalization; encode hop count, protected policy/source/callback/read/cancellation outcomes, total deadline, and exactly-one release counters in the one runtime corpus source.

## 4. Registration, documentation, and bounded evidence

- [ ] 4.1 Register `silk.http_redirect`, refresh generated stdlib/catalog surfaces, and publish generated plus prescriptive documentation covering defaults, replay, status transitions, URI/fragments, headers, limits, deadlines, ownership, errors, proxy composition, explicit exclusions, and deliberate differences from the pinned Zig implementation; illustrate HTTP/HTTPS flows in prose or by referencing the shared acceptance source without adding separately compiled documentation fences.
- [ ] 4.2 Integrate all structured redirect assertions into one existing HTTP-worker `Analysis` snapshot and its one source program, including pure StaticEvaluation results and ownership/error-row checks; do not add a standalone worker, duplicate program analysis, stress/timing case, or fresh-process check.
- [ ] 4.3 Integrate one compact table-driven redirect case into the shared native acceptance corpus and reuse that exact source for exactly one named LLVM-to-Wasm portability leg, covering only the distinct method/target/header/replay/drain/deadline/release signals and relying on existing proxy/TLS suites for transport identity; derive and record the fixture expectations independently from RFC 9110/3986 rather than from the pinned Zig implementation.
