## 1. Scoped session foundation

- [ ] 1.1 Add terminal idempotent `BufferedDuplex.close` with no implicit flush, exact typed close failure, and one-attempt state; extend the existing buffered-byte scripted case to verify pending bytes are abandoned and repeated/enclosing close releases the provider once.
- [ ] 1.2 Add `silk.websocket_server` limits, states, operations, owned control/close metadata, exact error/failure actors, and pure constructors/getters; extend one existing HTTP/WebSocket analysis source to verify positive limit/default/close-data contracts and exact invalid-limit/code/UTF-8 outcomes without another Analysis snapshot.
- [ ] 1.3 Implement higher-ranked `withServer` over the exact upgraded `BufferedDuplex`, excluding ambient `ByteDuplex` and preserving generic A/E/R rows through nonparking release; add focused structured ownership evidence for session/channel escape, duplication, and callback error-row preservation in the existing worker.

## 2. Bounded inbound frames

- [ ] 2.1 Implement fixed-scratch incremental header decoding for mask, RSV, FIN/opcode, canonical 7/16/64-bit lengths, high-bit, control, checked narrowing, and policy/caller bounds; cover the distinct 125/126/65,535/65,536 and negative header purposes with one compact table in the existing WebSocket source.
- [ ] 2.2 Implement masked Text/Binary reads directly into caller storage, complete-payload UTF-8 admission, unusable partial-prefix failure semantics, and terminal `MessageTooLarge`/`BufferTooSmall`; verify RFC Hello across fragmented transport reads plus exact size, truncation, timeout, and invalid-UTF-8 outcomes in that same program.
- [ ] 2.3 Implement copied Ping/Pong metadata and automatic byte-identical flushed Pong before Ping publication, with Pong surfaced rather than skipped; verify response order, automatic-response failure, no event publication, and no retry using the existing scripted duplex and audit signal.

## 3. Output and close lifecycle

- [ ] 3.1 Implement validated unmasked Text/Binary/Ping/Pong writes with minimal headers, whole-call payload borrows, exact accepted-prefix accounting, and one final flush; verify RFC Hello, boundary headers, short writes, partial failure, and structured cancellation without a new runtime program.
- [ ] 3.2 Implement exact incoming/outgoing Close payload and code rules, Open peer reply, 1010-to-1000 normalization, simultaneous-close behavior, and publish `PeerClose` only after flush and terminal close; cover empty/code+empty-reason/invalid-code/invalid-reason/reply-failure cases in the shared WebSocket table.
- [ ] 3.3 Implement idempotent `sendClose` and iterative `finishClose` with unchanged absolute deadline, aggregate frame/wire budgets, bounded data discard, Ping/Pong handling, truncation, and sticky primary-error preservation; verify normal, simultaneous, timeout, limit, malformed, cleanup-failure, and callback-cancellation release signals in one consolidated existing-source lifecycle scenario.

## 4. Delivery surface and economical evidence

- [ ] 4.1 Register `silk.websocket_server` in the standard-library manifest and add complete public source documentation, examples, unfragmented-profile warning, and pinned RFC/independent-vector provenance; verify only the targeted standard-library registration and public-doc-comment checks, with no generated documentation or broad pipeline run.
- [ ] 4.2 Refactor the existing `websocketUpgradeAcceptance.ts` support only as needed so one program performs upgrade plus frame/session cases, and reuse that identical source for the existing native corpus entry and its one LLVM-to-Wasm leg; verify only the exact selected feature-specific analysis/native/Wasm tests and add no file, worker, physical socket, TLS vector, backend matrix, stress/timing case, or fresh-process execution.
