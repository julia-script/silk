## 1. Parser representation and state

- [x] 1.1 Add `silk.http_head` public limits, progress, state, component, reason, and error values plus affine request/response parser construction; focused structural assertions verify fixed owned byte/index capacity, checked budgets, and precise public signatures.
- [x] 1.2 Implement the shared incremental CRLF scanner and parser state transitions; focused fixtures verify every discriminating split point, one-byte delivery, exact per-call consumption, suffix preservation, failure poisoning, truncation, and allocation-free reset.

## 2. Request and response semantics

- [x] 2.1 Implement strict request-line and field parsing through the delivered HTTP value/target/header actors; focused fixtures verify separator, Host, target-authority, OWS, duplicate/order, control-byte, offset, count, and byte-limit behavior.
- [x] 2.2 Implement strict response-line and field parsing; focused fixtures verify versions, status bounds, required reason separator, empty and obs-text reasons, line-ending/obs-fold rejection, offsets, and exact limit failures.
- [x] 2.3 Publish completed borrowed heads and delegate explicit owned copies; ownership-analysis fixtures reject head escape/reset conflicts and runtime evidence proves an owned copy survives parser release.

## 3. Serialization and public integration

- [x] 3.1 Add checked request/response serialized-size and memory-write operations; focused fixtures verify exact bytes, empty reason/value handling, repeated field order, semantic reparse, overflow, and unchanged insufficient destinations.
- [x] 3.2 Register the actor in the standard-library manifest and generated public catalogs, extending focused module-surface assertions for the exact exported API.
- [x] 3.3 Add concise public reference and module documentation for ownership, limits, exact progress, strict syntax, errors, target availability, and the boundary with framing/transport actors; documentation validation covers the examples.
- [x] 3.4 Add the minimum shared native and intended LLVM-to-Wasm acceptance cases needed to prove portable runtime behavior without a per-split compiler matrix or bespoke native binary.
