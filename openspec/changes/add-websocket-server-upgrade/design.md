## Context

See proposal.md for motivation. Merged main includes `http_server.withUpgrade`, which validates generic Upgrade, serializes and flushes the response, changes the phase, and lends the same buffered channel. Its enclosing scoped provider owns close throughout. Parsed request heads borrow parser storage.

## Goals / Non-Goals

Preserve the generic server lifetime and callback rows, keep peer validation pure, and bound all handshake-owned allocations. Do not add another HTTP parser, Base64 codec, transport owner, frame layer, compression engine, or TLS server.

## Decisions

- Represent the decision callback as a nominal higher-ranked `DecisionHandler`. The wrapper owns
  the policy context and lends it with the offer to `decide`, whose `Decision` borrows for that
  call. Copy the private response plan before ending either borrow. This binds configuration and
  request metadata to an explicit input loan instead of inferring an unrelated configuration
  lifetime from a conformance bound. Optional headers avoid an empty borrowed array for the common
  no-extra-headers decision. Rejection supplies a status and optional fields; the wrapper selects
  the request version and generates zero-body framing.

- Borrow request metadata in `Offer`; iterate repeated fields without allocating. Validate singleton counts, token grammar and duplicates before application code.
- Use explicit application decision and channel callbacks. Copy accepted metadata and response values into a private bounded plan before ending request-head borrows. No public plan constructor or validation Boolean can authorize a different request.
- Split shared HTTP response selection from body-encoder construction so body-free rejection validates the same framing without allocating unused chunk/trailer buffers outside the handshake budget.
- Reuse shared HTTP values and generic server handoff. This preserves serial request ownership and unread suffix rather than introducing a second socket or buffer path.
- Keep typed inspection errors independent from writing. Deterministic HTTP mappings are available for rejection. Application rejection is an ordinary HTTP response; callback errors keep their original channels.
- Defaults and zero limits follow the issue contract. Preflight response and allocation sizes with checked arithmetic, including header/index storage, before reserving memory or output.
- Use existing acceptance infrastructure for runtime and transport proofs; narrow structured analysis proves ownership, and the public RFC example shares declaration analysis. The ordinary runtime API makes no compile-time execution claim; StaticEvaluation is reserved for APIs explicitly available in that phase.

## Risks / Trade-offs

Application rejection returns `Outcome<A>.Rejected {status}` after the ordinary response is sent;
successful handoff returns `Outcome<A>.Upgraded {value}`. Rejection is not an Effect failure.
The existing private server phase consumes the HTTP disposition while the request remains a scoped
loan. Pre-output failures leave that disposition active for a caller-selected rejection.

- Borrowed decision metadata can retain the request loan → build owned response data before invoking the mutable server handoff.
- Repeated subprotocol scans can be quadratic → bound protocol count and token length explicitly; avoid an allocating token index.
- Serialization can allocate extra header storage → include exact capacities in the plan budget and preserve inherited server limits.
- Protocol errors after output cannot recover → delegate terminal output and close semantics to the existing server scope.

## Reference comparison

RFC 6455 sections 4.2–4.3 define handshake and extension grammar; RFC 6454 defines serialized origins. Strict canonical Base64 is the selected Silk profile. The issue's pinned Zig Server.zig comparison is precedent for buffered reuse only: last-header-wins, assertion validation, and unflushed response behavior are not adopted. No Zig negative suite is claimed as an oracle.
