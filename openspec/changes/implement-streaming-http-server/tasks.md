## 1. Public connection and request lifecycle

- [x] 1.1 Add `silk.http_server` public phases, limits, body/reuse policies, progress, and closed
      error family; verify one canonical public-import analysis exposes only ordinary-source actors and
      preserves generic callback channels.
- [x] 1.2 Implement `withConnection` over one explicit concrete provider and bounded buffered
      session; verify all limits precede allocation/I/O, the provider cannot rebind, nested values cannot
      escape, and structured exits close exactly once without implicit flush or drain.
- [x] 1.3 Implement serial `withRequest` head admission, parser reset, clean-end/truncation
      distinction, and exact buffered suffix retention; verify split heads and a coalesced two-request
      fixture cannot dispatch the second request early.

## 2. Request body, expectations, and response output

- [x] 2.1 Implement request framing selection plus streaming `readSome` and finite
      `discardRemaining`; verify method-independent bodies, payloads larger than tiny buffers, exact
      consumed/written progress, trailer failures, discard framing overhead, and close on unread body.
- [x] 2.2 Implement bounded Expect parsing and exactly-once automatic 100 output; verify mixed-case
      duplicate normalization, flush-before-read ordering, immediate unsupported 417 rejection,
      HTTP/1.0 behavior, and no wait for a rejected unsent body.
- [x] 2.3 Implement informational response validation and the shared finite budget; verify 100-199
      except 101, version/framing validation, automatic-count inclusion, atomic pre-output failure, and
      informational/final uniqueness.
- [x] 2.4 Implement final-head normalization and serialization plus nested `ResponseWriter` body
      driving and finish; verify all BodyMode variants, special-response exclusions, fixed boundaries,
      chunk trailers, close delimitation, short writes, exact progress, and no duplicate final/finish.

## 3. Persistence, failure, shutdown, and handoff

- [x] 3.1 Implement HTTP/1.0 and HTTP/1.1 persistence planning before output; verify close-token
      precedence, explicit 1.0 keep-alive, unknown-length 1.0 closure, unread-body closure, zero/last
      request bounds, and generated Connection indications.
- [x] 3.2 Implement recoverable pre-output validation and terminal post-output failure/cancellation
      transitions; verify an untouched request can choose another bounded response while partial output
      preserves its primary error and forbids fallback or replay.
- [x] 3.3 Implement explicit `finishConnection` flush, write-shutdown, bounded-drain, and close-state
      ordering; verify end/deadline/drain-limit outcomes and that cancellation finalization performs only
      nonparking close.
- [x] 3.4 Implement scoped generic Upgrade and CONNECT tunnel handoff; verify request/token/body and
      switching-response validation, exact coalesced suffix transfer, post-handoff HTTP unavailability,
      loan non-escape, and terminal failure after a partial switching response.

## 4. Native admission, acceptance, and documentation

- [x] 4.1 Add `silk.http_server_native.serveNext` over scoped listener accept and the portable
      connection actor; verify one invocation admits exactly one connection, keeps the accepted owner
      armed, and rejects public native admission on WebAssembly before backend/native-symbol discovery.
- [x] 4.2 Add one focused analysis file and one exported portable acceptance source, consolidating
      ownership/target claims at structured tiers and the runtime lifecycle matrix into one shared native
      plus intended LLVM-to-Wasm program without a redundant per-feature binary compilation.
- [x] 4.3 Add the streaming HTTP server reference page; verify examples use public imports and the
      page documents serial concurrency, borrowed lifetimes, bounds, Expect/reuse policy, output
      progress, graceful versus abortive cleanup, handoff ownership, and native target availability.
- [x] 4.4 Register both public modules and the shared native/Wasm/native-admission fixtures through
      the repository's generated/shared integration surfaces; verify generated publication and selected
      shared executions resolve the canonical module paths and expected exit sentinels.
