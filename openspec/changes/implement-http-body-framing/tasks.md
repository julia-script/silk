## 1. Public framing contract

- [x] 1.1 Add the `silk.http_body` public framing, selection, anomaly, limits, progress, completion, trailer-policy, and error types; verify a canonical public-import analysis resolves the intended surface without compiler-only recognition.
- [x] 1.2 Implement request and response receiving selection plus independent outgoing validation; verify focused structured/static cases cover HEAD, 1xx/204/304, CONNECT, 205, HTTP/1.0, conflicting fields, strict Transfer-Encoding, and duplicate/overflowing Content-Length.

## 2. Incremental decoder and trailers

- [x] 2.1 Implement bounded decoder acquisition and exact Empty, Fixed, CloseDelimited, and Tunnel state transitions; verify focused cases cover owned-capacity failure, zero progress, exact suffix preservation, tiny output, sticky EOF, truncation, and non-reusable close completion.
- [x] 2.2 Implement strict chunk-size, extension, payload, CRLF, and trailer phases with checked counters; verify a compact split-input matrix covers valid quoted escapes, malformed syntax, pending output at EOF, exact 14-byte example consumption, and all independently observable limit categories.
- [x] 2.3 Implement default and explicit trailer policy, advisory declaration validation, Connection-nominated exclusions, borrowed completed views, and fallible owned copies; verify policy failures, separate trailer provenance, and ownership rejection while views are live.
- [x] 2.4 Implement terminal decoder failure, reset, abandonment, and finite discard; verify later calls return InvalidState and bounded discard counts framing overhead without producing completion evidence after failure.

## 3. Incremental encoder and completion evidence

- [x] 3.1 Implement allocation-free Empty, Fixed, Chunked, and CloseDelimited encoding over preallocated working storage; verify Empty payload rejection, Tunnel rejection, fixed underrun/overrun boundaries, tiny output, and partial committed progress.
- [x] 3.2 Implement trailer finish initiation and immutable continuation; verify invalid trailers emit no finish prefix, continuations accept no replacement snapshot, and the terminating chunk and trailer terminator are emitted exactly once.
- [x] 3.3 Implement opaque borrowed completion evidence for delimited, close-delimited, and tunnel outcomes; verify active, failed, and abandoned states expose no positive evidence and source code cannot construct evidence directly.

## 4. Integration, acceptance, and documentation

- [x] 4.1 Register `silk.http_body` in the standard-library manifest and regenerate the catalog; verify the canonical `import silk.http_body` program analyzes through the public module path.
- [x] 4.2 Add focused compiler tests and one exported portable acceptance source; verify structured analysis and StaticEvaluation cover source contracts while the shared native corpus plus intended LLVM-to-Wasm leg cover runtime framing, trailers, limits, encoding, and exact boundaries without a redundant per-feature native test.
- [x] 4.3 Add the HTTP body-framing reference page and reference index entry; verify public examples compile and the page documents the strict profile, ownership, failures, limits, discard policy, and the limits of completion evidence.
