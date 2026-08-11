## Why

Silk needs a portable filesystem contract that applications can satisfy with native, Wasm-hosted,
virtual, test, or application-specific providers without changing calling source. Platform
mechanisms and provider implementations must remain separate so merely packaging the standard
library does not contaminate portable programs.

## What Changes

- Add provider-absolute normalized `Path` with explicit-base relative resolution and no ambient
  current directory.
- Add owned `FileInfo`, `DirectoryInfo`, and `DirectoryEntry` values plus an allocation-free
  `FileError` carrying a closed portable reason and optional numeric provider detail.
- Add the seven-operation whole-file `FileSystem` service: `readFile`, `writeFile`, `stat`,
  `listDirectory`, `createDirectory`, `removeFile`, and `removeDirectory`.
- Make every operation require `&mut FileSystem`, allowing recording, caching, and failure-injection
  providers while keeping provider replacement lexical and ordinary.
- Add source-defined helpers `createDirectoriesRecursively`, `writeFileWithParents`, and `exists`
  above the narrow service primitives.
- Specify complete-message write input without requiring atomic replacement, rollback, one physical
  write, or a portable public file handle.
- Preserve direct-Wasm pay-for-use: a program that uses no filesystem or supplies a pure user-defined
  service implementation requires no OS filesystem import.
- Require `add-returned-lexical-borrows` and `add-owned-bytes` before implementation.

## Capabilities

### New Capabilities

- `bootstrap-file-system`: Provider-absolute Path values, portable filesystem values and errors,
  the seven-operation service, helper semantics, and provider-independent behavior.

### Modified Capabilities

- `bootstrap-silk-stdlib`: Ship the portable actors as canonical ordinary Silk source with precise
  ownership, allocation, Effect, and tooling contracts.
- `bootstrap-backend`: Preserve ordinary service lowering and prove filesystem support is pay-for-use
  without FileSystem-shaped MIR or implicit Wasm imports.

## Impact

The change affects canonical standard-library source and manifests, ordinary nominal value and
service elaboration, ownership and allocation rows, evaluator/native/direct-Wasm acceptance fixtures,
tooling, examples, and documentation. It adds no compiler intrinsic, platform provider, hosted-Wasm
ABI, built-in virtual filesystem, public file handle, native path type, implicit cwd, String policy,
or backend filesystem operation. A separate `add-os-file-system-provider` change supplies the first
native implementation after this contract and generic target-restricted intrinsics exist.
