## Why

Silk needs file interaction for real applications and eventual compiler work, but defining the
common service in terms of Unix paths, native handles, or process state would make ordinary programs
non-portable to browser-hosted WebAssembly. The portable whole-file contract and the deliberate
platform-specific escape hatch must be separated before filesystem mechanisms leak into language
semantics.

## What Changes

- Add a portable normalized `Path` value distinct from String, source-module identity, and
  host-native path values.
- Add an owned `Bytes` actor over ordinary Silk storage so whole-file reads return a domain value
  rather than exposing Vector as the permanent filesystem API.
- Add an explicit `FileSystem` service for the smallest path-based whole-file and directory slice,
  using complete owned values, deterministic ordering, and no hidden working directory.
- Add a closed portable `FileError` reason model with optional provider-native diagnostic detail.
- Add native and deterministic in-memory providers and a direct-Wasm host-provider contract
  suitable for a browser virtual file system without changing calling Silk source.
- Add an explicitly lower-level `PlatformFileSystem` service boundary for native paths, handles,
  mapping, locking, and metadata that have no honest portable semantics; ordinary standard-library
  APIs do not depend on it.
- Integrate FileSystem values and operations with canonical standard-library source, Effect
  requirement/provision, editor tooling, labs, and evaluator/native/direct-Wasm acceptance.
- Keep open handles, streaming, watchers, mapping, locking, broad native metadata, implicit current
  directories, and process environment access outside the portable slice.

## Capabilities

### New Capabilities

- `bootstrap-file-system`: Portable Path and FileError values, whole-file and directory operations,
  native/in-memory/hosted-Wasm providers, deterministic semantics, and the lower-level
  PlatformFileSystem boundary.

### Modified Capabilities

- `bootstrap-silk-stdlib`: Ship canonical navigable Path and FileSystem source and provider-facing
  contracts without compiler-known filesystem actor names.
- `bootstrap-backend`: Carry portable filesystem operations through native and direct-Wasm host
  boundaries without embedding one platform implementation in MIR.

## Impact

The change affects canonical Silk standard-library sources, value/type/layout and service
elaboration, evaluator providers, native runtime shims, direct-Wasm imports, test providers,
tooling presentation/navigation, labs, and differential acceptance. It does not migrate the
TypeScript stage-0 source resolver, select a self-hosting module, add general FFI, or expose native
file handles through the portable API. The artifacts are reconciled with source-defined `service`
contracts, static `interface` conformances, and the sealed `Intrinsic` namespace. Implementation
waits for `establish-minimal-intrinsic-boundary` to archive and adds no FileSystem-named intrinsic
or MIR operation.
