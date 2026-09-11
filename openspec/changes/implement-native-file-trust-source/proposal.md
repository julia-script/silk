## Why

Silk can now represent owned trust snapshots behind a replaceable `TrustSource`, but native applications cannot yet load an explicitly selected PEM trust file without building an unbounded or platform-policy-specific adapter themselves. The first native provider must make acquisition finite, preserve filesystem error and cleanup semantics, and remain an explicit file-membership mechanism rather than implying host trust-policy equivalence.

## What Changes

- Add the native-only `silk.native_file_trust_source.NativeFileTrustSource` provider with owned, validated root and normalized portable path configuration.
- Read each load through `NativeFileSystem` using reusable 4096-byte scratch storage, an inclusive caller input budget, and a separate one-byte EOF probe at the limit.
- Close the descriptor before strict PEM decoding and publication, preserving a read, limit, or allocation failure over cleanup failure while surfacing close failure after otherwise successful acquisition.
- Decode complete bounded input through `TrustSnapshot.fromPem`; publish no partial snapshot or implicit cache, and reopen the configured path on every load.
- Select the provider only for Darwin ARM64/system libc and GNU Linux x86-64 or ARM64; keep it absent on Wasm and no-libc supplies.
- Add synthetic fixture, target-selection, ownership, fault-ordering, reload, documentation, manifest, generated, and CI evidence without consulting a live host root store.

## Capabilities

### New Capabilities

- `native-file-trust-source`: Explicit native PEM-file trust configuration, bounded acquisition and atomic snapshot publication, exact error precedence, reload ownership, target availability, and deterministic conformance evidence.

### Modified Capabilities

None.

## Impact

The change adds one ordinary-Silk standard-library actor and its generated package/documentation registrations. It reuses `silk.native_filesystem` as the sole libc boundary and depends on the existing `TrustSource`, `TrustSnapshot`, `Path`, `FileError`, and allocator contracts. Compiler tests and native conformance fixtures gain narrowly scoped evidence for selected-source availability, bounded reads, failure precedence, and independent snapshots; no compiler intrinsic, foreign declaration owner, default path, host policy integration, cache, search, watcher, or directory scan is introduced.
