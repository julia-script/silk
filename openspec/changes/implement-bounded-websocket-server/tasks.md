> Scope update (2026-09-15): Julia requested implementation-only completion, the smallest
> feature-specific test, and no further CI, broad-suite, documentation-generation, or Wasm checks.
> The selected native acceptance test was stopped after three silent minutes, so these completed
> checkboxes record source implementation, not a passing verification run.

## 1. Scoped session foundation

- [x] 1.1 Add terminal idempotent `BufferedDuplex.close` with no implicit flush, exact typed close failure, and one-attempt state; extend the existing buffered-byte scripted case for pending-byte abandonment and single provider release.
- [x] 1.2 Add `silk.websocket_server` limits, states, operations, owned control/close metadata, exact error/failure actors, and pure constructors/getters.
- [x] 1.3 Implement higher-ranked `withServer` over the exact upgraded `BufferedDuplex`, excluding ambient `ByteDuplex` and preserving generic A/E/R rows through nonparking release.

## 2. Bounded inbound frames

- [x] 2.1 Implement fixed-scratch incremental header decoding for mask, RSV, FIN/opcode, canonical 7/16/64-bit lengths, high-bit, control, checked narrowing, and policy/caller bounds.
- [x] 2.2 Implement masked Text/Binary reads directly into caller storage, complete incremental UTF-8 admission, unusable partial-prefix failure semantics, and terminal `MessageTooLarge`/`BufferTooSmall`.
- [x] 2.3 Implement copied Ping/Pong metadata and automatic byte-identical flushed Pong before Ping publication, with Pong surfaced rather than skipped.

## 3. Output and close lifecycle

- [x] 3.1 Implement validated unmasked Text/Binary/Ping/Pong writes with minimal headers, whole-call payload borrows, exact accepted-prefix accounting, one final flush, and sticky terminal failure handling.
- [x] 3.2 Implement exact incoming/outgoing Close payload and code rules, Open peer reply, 1010-to-1000 normalization, simultaneous-close behavior, and publish `PeerClose` only after flush and terminal close.
- [x] 3.3 Implement idempotent `sendClose` and iterative `finishClose` with unchanged absolute deadline, aggregate frame/wire budgets, bounded data discard, Ping/Pong handling, truncation, and sticky primary-error preservation.

## 4. Delivery surface

- [x] 4.1 Register `silk.websocket_server` in the standard-library manifest and add complete public source documentation, examples, unfragmented-profile warning, and pinned RFC/independent-vector provenance.
- [x] 4.2 Refactor the existing `websocketUpgradeAcceptance.ts` support so one program performs upgrade plus frame/session cases and remains shared by the native and LLVM-to-Wasm acceptance topology, without adding a file, worker, physical socket, TLS vector, backend matrix, stress/timing case, or fresh-process execution.
