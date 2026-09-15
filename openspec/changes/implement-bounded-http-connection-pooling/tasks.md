## 1. Streaming client and owned acquisition seams

- [ ] 1.1 Add a read-only `http_client` reuse-eligibility result that positively checks Ready phase,
      request-budget headroom, persistence, pending output/transfer state, and an empty retained suffix;
      cover exact eligible, `maxRequests = 1` exhaustion, unsolicited-suffix, incomplete,
      close-delimited, upgrade, and CONNECT-transfer decisions in the shared HTTP analysis/runtime
      sources.
- [ ] 1.2 Add the client/content-owned live-exchange drain-and-finish operation with admitted
      65536-byte default and 1048576-byte maximum aggregate wire cap, finite deadline clamped to the
      request deadline, trailer and decoder completion, and precise cap/timeout/framing/read/decoder
      outcomes; extend the existing HTTP runtime corpus with only the distinct complete, cap, malformed,
      timeout, and decoder-failure cases.
- [ ] 1.3 Add the smallest ordinary-source native owned-acquisition boundary for supported direct
      TCP, Unix, and HTTPS origins. Reuse the existing resolver/socket/TLS owners, arm one concrete
      release guard from socket acquisition through HTTP-owner publication, consume caller-prepared
      trust for HTTPS, construct the pooled `Connection` with no overall deadline, reject expired or
      unsupported acquisition before publication, and add structural native plus one reused TLS-vector
      witness proving exact close and no leaked acquisition/handshake deadline.

## 2. Pool values, identity, and frozen context

- [ ] 2.1 Add the `silk.http_connection_pool` configuration, duration, pool/handle error, capacity,
      collection, and reuse result values with checked defaults and admission. Cover zero/excess limits,
      nonpositive timeout, arithmetic overflow, and exact 1/1024 and idle/per-origin capacity boundaries
      in the one shared positive analysis source and shared pool runtime table.
- [ ] 2.2 Implement the owned conservative connection key for normalized original HTTP/HTTPS origin,
      effective port, route endpoint/mode, proxy credential identity, tunnel authority, Unix path, and
      sealed provider/security context. Cover DNS casefold/default-port equality plus every distinct
      noncoalescing origin, route, credential, trust, ALPN, version, provider, and Unix-path signal in
      one compact key table without resolving or opening a connection.
- [ ] 2.3 Implement private pool-context construction that freezes provider, HTTP, routing, TLS/ALPN,
      origin, credential, and prepared-trust policy outside shared state. Add fallible handle copying
      that copies bounded context and `TrustSnapshot` before publishing a Shared alias; prove allocation
      failure publishes no partial handle and a mutated backing trust source cannot affect later opens.

## 3. Bounded local state and deterministic admission

- [ ] 3.1 Implement preallocated pool slots and synchronous `Shared.withMut` transitions for
      `CONNECTING + LEASED + IDLE` global/per-origin accounting, checked counters, lazy idle expiry, and
      extraction of owned work. Add structural assertions that shared callbacks contain no allocator,
      clock, suspension, close, connector, or caller callback and do not nest aliased Shared access.
- [ ] 3.2 Implement atomic fail-fast checkout selection: newest compatible idle first, otherwise one
      reservation, same-origin eviction as the only per-origin relief, and oldest global idle eviction
      with stable insertion-sequence ties. Exercise global/per-origin caps, `maxIdle = 0`, PoolFull,
      recent selection, stable oldest eviction, and lazy expiry in the shared runtime table.
- [ ] 3.3 Implement explicit collection that samples the monotonic clock before shared mutation,
      extracts all expired owners without allocation, and physically closes them afterward. Reuse the
      runtime table to prove idle-only expiry, no checked-out interruption, exact counts, and exactly-once
      closes.

## 4. Scoped checkout, publication, and closure

- [ ] 4.1 Add the higher-ranked connector/use-handler contracts and one pool checkout operation with
      exact generic success, connector, client, callback, and requirement rows. Keep each reservation or
      idle owner in one nonparking guard outside Shared and consolidate lease/connection escape,
      duplication, and provider-row failures into one frontend-only negative snapshot.
- [ ] 4.2 Implement protected release so success samples its return timestamp before publishing only
      a client-eligible owner, while typed failure, defect, cancellation, stale I/O, ineligibility, or
      closed-pool publication refunds and closes exactly once. Cover acquisition, allocation, source,
      callback, transport, and cancellation outcomes with reservation/lease/open/close counters in the
      shared runtime table.
- [ ] 4.3 Implement explicit close and scoped pool finalization as one idempotent nonparking state
      transition that marks closed, extracts and closes idle owners, rejects new and late publication,
      preserves live leases, and closes them on return. Exercise idle + opening + leased closure,
      surviving-handle PoolClosed, repeated close, and original-outcome preservation in the same table.

## 5. Deadline and route composition

- [ ] 5.1 Thread the current request's unchanged absolute deadline through reservation, supported
      acquisition, exchange body/trailers, and explicit drain while applying the optional
      acquisition-only bound solely to opening and retaining the TLS handshake-duration cap. Add a
      falsifiable clock case where a second request succeeds after the first acquisition/request
      deadlines and a separate expired opening publishes nothing.
- [ ] 5.2 Add direct native pool construction and convenience operations over the same connector and
      checkout path for TCP, Unix HTTP, and HTTPS. Preserve native hostname `UnsupportedDeadline`,
      unsupported-target preflight, precise resolver/socket/TLS/trust rows, original-origin TLS identity,
      and request-scoped authorization; prove supported numeric/Unix finite deadlines structurally and
      through the existing shared native fixtures.
- [ ] 5.3 Compose sealed JUL-199 route identity when available so Direct, ForwardProxy, and
      ConnectTunnel keys remain distinct and forward reuse stays per original origin. Reject unavailable
      or unsupported proxy composition before contact, preserve credential secrecy, and extend existing
      proxy preparation evidence rather than adding a proxy/TLS/socket matrix; direct pooling must remain
      independently usable.

## 6. Bounded integrated evidence and documentation

- [ ] 6.1 Add pool declarations, exact public error/requirement-row witnesses, client/native seam
      reachability, and pure configuration/key/state checks to one existing positive HTTP `Analysis`
      source and lowered MIR assertion; add only the single consolidated frontend-negative ownership
      source from task 4.1, with no additional realization or worker.
- [ ] 6.2 Add one compact table-driven `http-connection-pool` program to the shared native acceptance
      corpus covering only distinct capacity, key, selection, expiry, reuse, drain, deadline, closure,
      typed-failure, and release/cancellation signals. Reuse that exact source for exactly one named
      LLVM-to-Wasm portability leg and reuse one existing HTTPS/trust vector; add no stress, timing,
      fresh-process, per-case compilation, worker, or transport matrix.
- [ ] 6.3 Register `silk.http_connection_pool`, refresh canonical generated standard-library/catalog
      artifacts, and publish public doc comments plus a prescriptive reference covering configuration,
      local synchronization, keys, trust freezing, acquisition/request/idle/drain deadlines, reuse,
      eviction, closure, errors, native/proxy composition, and explicit exclusions, with examples that
      reuse the shared acceptance source rather than separately compiled fences.
