## Why

Silk's authenticated TLS client currently exposes only a transport-independent partial-byte driver. Applications cannot yet bind it to reliable byte I/O with deadlines, exact short-transfer ownership, or one scoped connection lifecycle that closes on structured cancellation.

## What Changes

- Add the exclusive `ByteDuplex` service, precise `ByteIoError` and `ReadTransfer` results, and a deterministic scripted memory provider for partial reads, partial writes, suspension, deadlines, EOF, errors, and cancellation.
- Add a scoped TLS connection adapter that acquires one duplex lease, trust snapshot, wall-clock instant, allocator, random provider, and monotonic handshake deadline before it publishes an authenticated connection.
- Add application read, write, flush, directional shutdown, peer-close, truncation, and final close behavior that preserves every acknowledged byte boundary of the existing TLS client.
- Add the smallest target-neutral intrinsic and ordinary-source Effect combinator needed to run a nonparking synchronous finalizer on success, typed failure, and structured Execution cancellation while preserving the original outcome. Fatal traps remain outside this guarantee.
- Register and document the new source modules, generated reference pages, deterministic acceptance evidence, and exact CI selection without adding sockets, DNS, ambient trust, or a general Stream dependency.

## Capabilities

### New Capabilities

- `scoped-byte-duplex-tls-connection`: Defines partial reliable-byte transport, deterministic memory driving, authenticated scoped TLS connection behavior, deadlines, exact ownership, cancellation-safe finalization, and economical cross-target evidence.

### Modified Capabilities

None.

## Impact

The change affects the compiler's sealed intrinsic catalog and Effect lowering only for generic cancellation-safe synchronous finalization. Ordinary Silk source owns all transport policy and TLS adapter behavior. The standard-library manifest, generated embedding, documentation index, reference pages, acceptance support, corpus/target coverage, and CI selection gain the new modules and evidence. The provisional implementation is based on JUL-187 draft PR #422 head `9e2e9a893078743d90d5215828b3610763418f0c` and must be rebased and reverified on JUL-187's final verified head before handoff.
