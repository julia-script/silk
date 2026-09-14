## Context

Work base e253eaa219856d3bc5809845cac5562d88cf7cf7 delivers the HTTP foundation stack. `http_head` owns bounded parsers/serialization; `http_body` owns exact prefix framing and completion; native sockets own physical descriptors. TLS now owns its concrete provider and already clamps an external deadline after trust acquisition. The issue's historical borrowed-TLS evidence is superseded.

## Goals / Non-Goals

**Goals:** compose source-owned actors with exclusive scoped connection/exchange authority, preserve rich transport errors and committed progress, and make admission decisions before wire output.

**Non-Goals:** retries, redirects, pooling, new content codecs, automatic decoding negotiation, collection, new compiler-recognized library operations, or a second TLS pump.

## Decisions

- Use a source HttpTransport service with explicit plain/secured adapters and bounded session storage. A generic ByteDuplex-only adapter would erase ConnectionError, so preserve that distinction at the transport boundary.
- Retain the concrete transport and all input/output/parser state in an owned session. Scoped views borrow that session; a later pool can retain a completed session without callback borrows.
- Keep one private exchange phase machine and per-message parser/encoder/decoder; immutable head loans prevent mutation while live. Completion combines both framing boundaries and persistence rather than status code.
- Use higher-ranked context handlers where captured affine setup cannot express a reusable closure; follow the delivered server/buffered context contracts.
- Keep origin identity and request target construction independent of native resolution. Reuse the synchronous native resolver refusal before hostname dispatch, and pass absolute marks throughout.
- Implement Basic in its own pure actor over Base64; request policy owns plaintext opt-in and authority binding.
- Scope every logical lease with the existing nonparking bracket; mark terminal before fallible work and only restore reuse with proven completion. Never flush or drain from cleanup.

## Risks / Trade-offs

- Generic loan and provider requirements are compiler-sensitive → structured analysis plus one composed native/Wasm execution, with no compiler spelling privilege.
- Rich TLS failures do not fit ByteDuplex's error row → retain a source transport boundary rather than stringify causes.
- Native hostname lookup is synchronous → refuse overall hostname deadlines before dispatch, document None as unbounded.
- Borrowed response storage is reused → document invalidation and rely on exclusive borrowing; explicit fallible copy provides ownership.
- Informational and body processing can amplify wire work → independent finite wire/count budgets and unchanged absolute deadlines.

## Response content provenance

Expose the existing content/framing core as an owned incremental decoder, leaving transport errors at the HTTP session boundary. A client exchange may select this decoder exactly once, before any raw read or discard. Build its plan only from that exchange's current head and request method. Completion requires codec validation and the framed boundary, and unread suffixes stay with the session. The low-level content reader continues to use this same core.

## Runtime provider identity

The secured adapter exposes the same admitted provider through distinct read and write loans. The compiler retains one provided call across these proof-only lifetime differences. Runtime call lookup must prefer exact provider matches, then compare runtime-equivalent provider shapes after semantic admission, preserving capability, role and nominal provider identity and rejecting ambiguous physical targets. This corrects lowering of ordinary source composition; semantic provider selection and lifetime checking remain strict.
