## Why

Standard-stream completion, interruption and native-error policy still lives in generated C and dedicated compiler operations. JUL-128 moves this behavior into ordinary selected Silk source over the verified descriptor ABI, while retaining lexical Writer and StandardInput services.

## What Changes

- Add hand-authored, target-selected Darwin/GNU descriptor and errno declarations with a validated descriptive PlatformCatalog record, exact header hashes and independent C fixtures.
- Implement bounded partial-write completion and interrupted reads in source. Empty writes and flush perform no foreign call. Zero-capacity reads return Filled(0) without claiming EOF or calling read.
- Keep process descriptors borrowed. The input provider latches a proven nonempty-read EOF to satisfy the service's existing permanent-end contract; construction never reads.
- Remove both stream intrinsics, HostWrite lowering, symbol reservations and generated adapters, and migrate every current consumer. Retain the independent generated reporting loop for JUL-130.

## Capabilities

### New Capabilities

- `source-standard-streams`: verified selected descriptor boundaries, source-owned transfer/error policy, borrowed standard descriptors and complete deletion of privileged stream paths.

### Modified Capabilities

None. The new contract explicitly defines previously unsupported zero-capacity reads while preserving the existing permanent-end service promise.

## Impact

Compiler intrinsic/HIR/MIR/backend inventories; ordinary Writer/StandardInput providers and selected platform modules; formatter/logger/examples; shared native corpus; descriptor ABI conformance; public documentation. No owned descriptor, filesystem, terminal, raw Linux or new Wasm host policy is introduced.
