## Why

Validated HTTP heads do not determine where a message body ends. A single bounded framing owner is needed so clients and servers cannot consume bytes from the next message, accept ambiguous length metadata, or authorize connection reuse after incomplete chunked or close-delimited input.

## What Changes

- Add pure request and response framing selection for empty, fixed-length, chunked, close-delimited, and tunnel bodies, including strict HTTP/1.0 and HTTP/1.1 precedence and explicit non-reusable anomalies.
- Add allocation-free incremental decoding with exact consumed and written progress, sticky EOF, bounded chunk syntax, strict CRLF, terminal failures, and completion evidence that distinguishes reusable delimitation from close-delimited completion and tunnel handoff.
- Add bounded trailer declaration and field validation with a non-overridable forbidden-name set, an explicit safe-name policy, borrowed completed views, and fallible owned copies.
- Add bounded incremental encoding for empty, fixed-length, chunked, and close-delimited bodies, including immutable finish snapshots, exact fixed-length underrun and overrun failures, and exactly-once chunk terminators.
- Add finite discard/abandonment semantics and typed failures carrying wire offsets plus committed per-call and cumulative progress.
- Document and verify the strict portable framing profile with structured compiler evidence and a shared native acceptance program suitable for the LLVM-to-Wasm runtime leg.

## Capabilities

### New Capabilities

- `http-body-framing`: Strict, bounded HTTP/1 message-body framing selection, incremental decoding and encoding, trailer policy, completion evidence, and discard semantics.

### Modified Capabilities

None.

## Impact

- Adds the public `silk.http_body` standard-library module and its generated registration.
- Builds on the validated HTTP value and head APIs from `silk.http`, `silk.http_headers`, and `silk.http_head`; it does not perform transport I/O or content decoding.
- Adds focused compiler tests, a shared portable acceptance source, and public reference documentation.
- Enables the streaming HTTP client, server, and bounded content-decoding workstreams to share one framing and persistence policy.
