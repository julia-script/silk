# Proposal

## Why

Native artifact and runtime-object caches each own a near-identical byte-store protocol, while the
disk implementation is coupled to `NativeToolchain` errors and raw filesystem paths. Semantic
persistence would either inherit those native concerns or create a third storage implementation.

## What Changes

- Add an Effect `Storage` service for bounded reads and atomic publication of opaque records.
- Add memory and filesystem layers with the same logical namespace/key addressing contract.
- Validate addresses and record limits, preserve caller ownership of all byte arrays, and expose
  distinct typed failures for invalid requests, oversize records, reads, and publication.
- Use unique same-directory temporary files plus atomic replacement for filesystem publication.
- Migrate artifact-cache and runtime-object-cache owners to `Storage` while retaining their own
  envelopes, admission rules, policies, error translation, and statistics.
- **BREAKING**: remove the superseded `ArtifactCache` and `RuntimeObjectCache` byte-store
  interfaces and their private memory/disk implementations.

## Capabilities

### New Capabilities

- `compiler-storage`: Provider-independent bounded record storage with memory and atomic-filesystem
  layers.

### Modified Capabilities

None.

## Impact

This changes compiler package exports, native cache construction/injection, Driver and Linker cache
use, filesystem cache publication, and focused storage/native cache tests. Final executable
publication, subprocess files, toolchain work directories, semantic validity, and cache envelope
interpretation remain with their existing owners.
