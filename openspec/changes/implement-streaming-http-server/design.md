## Context

See `proposal.md` for motivation and `specs/streaming-http-server/spec.md` for the observable
contract. The delivered substrate already provides: affine concrete `ByteDuplex` providers;
`BufferedDuplex` sessions whose finalizer closes without flushing; allocation-owning incremental
request-head parsers; transport-independent body `Decoder`/`Encoder` owners; and scoped native
`Listener.accept`/`withAcceptedContext`. The companion
`support-conditional-effect-witness-inference` change supplies the general frontend proof needed
to select nominal generic contexts; no privileged actor is needed.

The server must join those pieces without weakening their ownership proofs. Parsed head fields
borrow parser storage, body framing must stop before pipeline suffixes, output can be accepted into a
buffer before it reaches a provider, and the native accepted connection must remain owned by the
listener scope while the generic server runs.

## Goals / Non-Goals

**Goals:**

- Represent exchange legality in affine connection/request/writer state rather than caller-managed
  booleans.
- Keep the transport and all head/body views under nested higher-ranked callback loans.
- Reuse exactly one bounded input/output session and one request parser across serial requests.
- Reuse the existing parser, serializer, framing decoder/encoder, and listener contracts without
  copying their grammars or failure policy.
- Make the portable scripted path carry most runtime evidence and isolate native testing to accept
  ownership and ABI/target boundaries.

**Non-Goals:**

- Parallel requests, HTTP/2, routing, middleware, supervision, background accept loops, or detached
  tasks.
- Native TLS server handshakes, certificate configuration, HTTPS/wss serving, or destination dialing
  for CONNECT.
- Automatic body drain, automatic handler retries, implicit response generation after partial
  output, or timeout preemption of handler CPU.
- WebSocket-specific Upgrade validation; JUL-202 builds its handshake decision on the generic scoped
  handoff.

## Decisions

### 1. Two ordinary-source modules separate portable exchange from native admission

`silk.http_server` owns the generic protocol lifecycle. `silk.http_server_native` is a thin adapter
that accepts one `native_socket.Accepted`, enters `withAccepted`, and invokes the portable actor with
the accepted connection as its explicit provider. The portable module contains no native import and
therefore remains available to the intended LLVM-to-Wasm scripted corpus; the native module inherits
the listener module's target guards.

Alternative considered: put `serveNext` in the portable module behind conditional declarations.
Rejected because it would make a target-neutral import transitively depend on native libc surface
and duplicate the established standard-library target-selection boundary.

### 2. `withConnection` composes one buffered bracket around an affine connection state

`withConnection(transport, limits, callback)` validates server-only scalar bounds first, then calls
acquires one request parser and fixed response-head scratch before transferring the borrowed
provider and those owners into `withBufferedCapacityContext` with the configured read/write
capacities. A validation or allocation failure before that transfer leaves the caller-owned
provider untouched; after transfer, every structured exit runs the provider close bracket. The
connection stores the concrete buffered-session borrow, parser, phase, request count, peer
connection-token facts for the current exchange, and whether output may have escaped. The outer
buffered bracket remains the sole provider-close owner; server cleanup changes state and abandons
framing owners but performs no duplicated close.

Alternative considered: have `Connection` provide `ByteDuplex` as an ambient service. Rejected
because nested handler requirements could then select another provider and because buffering would
be bypassable. Every transport operation is routed explicitly through the stored concrete
`BufferedDuplex`.

### 3. `withRequest` builds a nested request owner after exact head completion

Request admission resets the retained parser, incrementally feeds `BufferedDuplex.peek`, consumes
only parser-reported accepted prefixes, and fills again only on NeedInput. Sticky transport end before
any accepted head byte returns `Option.None`; later end maps to typed truncation. A completed head is
used immediately to select body framing and acquire a fresh single-message `Decoder`, whose copied
policy/Connection exclusions allow the parser borrow to end. The callback receives
`Request<'request, P>` containing exclusive access to the connection, decoder, expectation state,
and request disposition. Its fallible `head()` accessor re-borrows the parser only while the
request phase remains active; copying through the shared HTTP value contract is the only way to
retain it independently. The body-completion and expectation accessors use the same phase guard.

The callback result is wrapped as `Option<A>` so clean end adds no redundant public result actor.
Response-writer release requires both explicit finish and successful callback return before
authorizing another request. A callback failure after finish remains terminal without replacing
the original failure or performing release I/O.

Callback error/requirement types stay generic and are unioned with `ServerError`/allocator/clock
requirements by the bracket signatures.

Alternative considered: copy every request head before dispatch. Rejected because it adds an
allocation and weakens the intended bounded buffer-backed lifetime; applications that need
independent ownership already have the explicit copy operation.

### 4. Request body driving bridges buffered input to the shared decoder without staging payload

`readSome` repeatedly offers the current unread slice to `Decoder.step`, consumes only its reported
wire prefix (including the committed prefix carried by a typed decoder failure), and returns after
payload progress, completion, or one necessary fill. It never consumes unwritten payload.
`discardRemaining` converts the smaller of the per-call budget and configured
`maxDiscardWireBytes` to an absolute decoder ceiling from the decoder's wire total at call entry,
counting syntax and trailers exactly as the decoder does.
Framing/trailer/truncation errors poison the connection; successful delimited completion records the
request as reusable, while close-delimited/tunnel outcomes cannot authorize ordinary server reuse.

Alternative considered: call `BufferedDuplex.discardAtMost` for fixed bodies and use a scratch
output for chunked bodies. Rejected because that would create two framing paths and either omit
chunk-syntax bytes from the budget or duplicate JUL-195 parsing.

### 5. Expect parsing is one bounded head scan with an explicit three-state latch

At admission, ordered Expect fields are parsed as comma-separated token members with optional outer
OWS. The request records `None`, `ContinuePending`, or `Rejected`. Only an HTTP/1.1 request with a
nonempty framed body can become ContinuePending; equivalent duplicates do not increment the
informational count. Before the first decoder-driving body operation, the server serializes and
flushes its fixed 100 head, increments the shared informational counter, then clears the latch. An explicit 100 uses the same sent latch, clears a pending automatic action,
and rejects a second 100 before output.
Rejected expectations allow only immediate final rejection and force closure; they never enter a
read/fill loop.

Alternative considered: send 100 during admission. Rejected because it removes the handler's
opportunity to inspect and reject the request before accepting the body.

### 6. Response validation is staged before one atomic final-head acceptance

`respond` first verifies phase, response version/status, selected `BodyMode`, shared outgoing
framing, special-response rules, current request-body disposition, reuse policy, and all count
arithmetic. It computes the final persistence decision before output. To make server-owned
Connection policy visible on the wire without forking the serializer, it builds a bounded temporary
ordered header vector from the caller fields after removing any caller Connection fields, appends
exactly one generated `Connection: close` or HTTP/1.0 `Connection: keep-alive` when required, creates
a validated borrowed shared `ResponseHead`, and invokes `http_head.writeResponseInto` into
preallocated scratch. Only after every check succeeds does it write the complete serialized scratch
through `BufferedDuplex.writeAll` and publish a `ResponseWriter` around the shared body `Encoder`.

The temporary vector and generated header are acquired before any output; allocation failure leaves
the request active. Once buffered output accepts the head, the connection enters Responding and any
later output failure/cancellation is terminal. The writer's `writeSome` and `writeAll` repeatedly
drive `Encoder.step` into bounded scratch and then the buffered output, keeping input accepted by the
encoder distinct from bytes drained to the provider. `finish` snapshots trailers through
`Encoder.beginFinish`, drains `continueFinish`, flushes the buffered response under the same deadline, and records exactly one
completion. The explicit flush makes the response visible before a reusable connection waits for
the next request; scope release still performs no I/O.

Alternative considered: require the caller to supply Connection headers. Rejected because request
count, unread body, errors, and HTTP/1.0 opt-in are server-owned facts that callers cannot safely
predict. Hand-serializing a second response grammar was also rejected.

### 7. Reuse is a closed decision over completed evidence

`ReusePolicy` selects normal protocol policy, explicit close, or HTTP/1.0 keep-alive opt-in; it never
forces reuse when evidence is absent. The decision combines request version, case-insensitive tokens
across all Connection fields (close wins), successful delimited request completion, the selected
self-delimited response mode, remaining request budget, and nonterminal connection state. Beginning
a final response with unread content sets CloseAfterResponse. The last admitted request therefore
gets its generated close indication before head serialization. Unknown-length HTTP/1.0 maps only to
close-delimited response encoding.

Alternative considered: decide reuse after writer finish. Rejected because the emitted head would
already have promised persistence and pipeline dispatch could race with late budget discovery.

### 8. Information, final output, and recovery use explicit monotone phases

Informational output validates the entire head into scratch before writing and increments its bound
only after its write and flush succeed. A pre-output validation or allocation failure leaves
RequestActive. A failure after framed response progress is offered to the bounded output changes
the connection to Failed and carries both the encoder totals and the exact scratch prefix accepted
by buffering. Generic handler failures escape unchanged; there is no automatic response mapper and
no second final-response path after Responding.

Alternative considered: catch all handler failures and emit 500. Rejected because a writer loan can
fail after an arbitrary prefix and HTTP/1 has no safe rollback delimiter.

### 9. Upgrade and tunnel reuse the same scoped buffer rather than returning transport ownership

`withUpgrade` validates request tokens, offered/selected protocol equality, empty request framing,
and a 101 head; `withTunnel` validates CONNECT, empty request framing, and a 2xx head. Both use the
same staged head path, force one flush, set Upgraded, and then lend the underlying
`BufferedDuplex<'session, P>` to a higher-ranked callback. Both the callback loan and the buffered transport view are fresh higher-ranked lifetimes; the callback does not depend on the caller's original transport lifetime. Its `peek` exposes the exact coalesced
suffix. The transition consumes the active HTTP disposition by moving the connection to Upgraded;
every request/head/writer operation is phase-guarded, so even a still-borrowed request value cannot
resume HTTP after the callback. The callback cannot escape the channel loan and the outer provider
bracket stays armed.

Alternative considered: return an owned transport or copy the suffix. Rejected because ownership
would detach from the close bracket and copying would change capacity/failure semantics at the
protocol boundary.

### 10. Explicit finish owns graceful ordering; finalizers stay nonparking

`finishConnection(deadline, maxDrainBytes)` is legal only after a final response or handoff has
completed. It calls buffered flush, then the concrete provider's canonical `shutdownWrite` through
the bound session, then repeatedly reads/discards at most the finite drain limit until end/deadline,
and finally marks Closed; the outer bracket performs the one provider close. The default
`shutdownDrainBytes` supplies a convenient policy bound but callers may choose a smaller explicit
bound. Errors from this explicit operation are returned normally. Cancellation or callback exit
does not call this path.

Alternative considered: graceful work in release. Rejected because release is specified
nonparking, must preserve the protected outcome, and cannot guarantee peer cooperation.

### 11. Verification is consolidated by semantic boundary

One focused test file creates one structured analysis snapshot for canonical imports, generic
channels, state opacity, loan escape/rebinding rejection, and native target exclusion. Pure
connection-token, expectation, and reuse planning functions use Evaluation where that tier
can falsify them. One exported scripted byte-provider source carries the portable runtime matrix
through shared native and LLVM-to-Wasm corpora: split/coalesced heads, tiny buffers, bodies,
expectations, short writes, errors, persistence, shutdown, and handoff suffixes. The native adapter
adds one deterministic accept/owner witness and one controlled loopback row through the existing
listener infrastructure rather than recompiling the portable program in a feature test.

## Risks / Trade-offs

- [The joined state machine can admit an illegal recovery edge] → Keep phases monotone, centralize
  transition guards, and give each portable sentinel a distinct state/output oracle.
- [Adding generated Connection fields could exceed caller header budgets] → Reserve the extra
  field in server limits, perform checked size/count validation before output, and return the precise
  value/owned-limit failure while the request remains active.
- [Decoder progress and buffered consumption can diverge] → Consume buffered input only after a
  successful decoder result or by the exact committed count carried in a typed body failure.
- [Encoder acceptance can precede provider acceptance] → Stage only within bounded encoder/output
  storage and report these coordinates separately in server errors; never call input consumed merely
  because the provider accepted a different prefix.
- [A slow peer can prolong graceful finish] → Require an explicit absolute deadline and finite
  drain bound; scope release remains abortive and nonparking.
- [The portable fixture can become default-suite expensive] → Use one source, one analysis, one
  shared native registration, and one intended Wasm registration; keep target and ownership failures
  at structured analysis tiers.

## Migration Plan

This is a green-field capability. Add the two ordinary-source modules, public generated entries,
focused tests, shared corpus registrations, and reference page together. JUL-202 consumes the scoped
upgrade contract directly. Rollback removes these ticket-local artifacts and shared registrations
before downstream server APIs land; there is no compatibility shim or prior server API to retain.
