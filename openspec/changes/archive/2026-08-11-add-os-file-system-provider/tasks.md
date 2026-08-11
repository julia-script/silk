## 1. Prerequisites and Protocol Inventory

- [x] 1.1 Confirm `add-portable-file-system` and `add-target-restricted-intrinsics` are complete before implementation begins.
- [x] 1.2 Add opaque affine `OsHandle` representation, ownership facts, and rejection tests for construction, copying, inspection, and reuse after consuming close.
- [x] 1.3 Add the unsafe native-only intrinsic catalog entries for confined file open/read/write, directory open/next, path inspection, directory creation, file removal, directory removal, and generic consuming close.
- [x] 1.4 Define and inventory the stable low-level reason codes, native `u32` output convention, partial-transfer behavior, handle-kind invariants, and directory retry protocol.

## 2. Analysis and Target-Neutral Lowering

- [x] 2.1 Implement semantic checking and diagnostics for unsafe acknowledgement, handle liveness/kind, initialized ranges, and consuming close.
- [x] 2.2 Add HIR/MIR operations and verifier rules using only scalars, slices, outputs, `Option`, `bool`, and `OsHandle`.
- [x] 2.3 Preserve reachable native-only availability and reject direct-Wasm use through the generic target-restricted intrinsic mechanism.

## 3. Evaluator Host Boundary

- [x] 3.1 Define the injected evaluator OS adapter with explicit file, directory, path, status, and close operations and no ambient process fallback.
- [x] 3.2 Implement adapter-backed evaluation with normalized reasons, native codes, partial transfers, retryable non-advancing directory entries, and consuming close.
- [x] 3.3 Add blocked evaluation behavior for a reachable OS operation without an injected host adapter.
- [x] 3.4 Add browser-bundle tests proving compiler/evaluator core imports no Node or ambient filesystem implementation by default.

## 4. Native Runtime and LLVM

- [x] 4.1 Implement root-confined native traversal that rejects symlinks, dot components, NUL, invalid encoding, and namespace escape on supported native platforms.
- [x] 4.2 Implement native file and directory handle operations with partial I/O, stable reason mapping, retryable directory buffers, and consuming fallible close.
- [x] 4.3 Lower validated OS MIR operations through LLVM and link only runtime symbols retained by executable intrinsic reachability.
- [x] 4.4 Add evaluator/native protocol parity and artifact pay-for-use tests, including proof that direct Wasm receives no invented filesystem ABI.

## 5. Ordinary OsFileSystem Provider

- [x] 5.1 Add canonical ordinary-source `OsFileSystem` with a constructor that copies and owns one absolute native root through `OutOfMemory ? &mut Allocator`.
- [x] 5.2 Implement explicit handle brackets that always attempt close, preserve primary failures, and surface close failure after otherwise successful work.
- [x] 5.3 Implement portable `readFile` and `writeFile` with complete read accumulation, partial-write loops, create-or-truncate behavior, and no rollback promise.
- [x] 5.4 Implement `stat`, sorted `listDirectory`, `createDirectory`, `removeFile`, and empty-only `removeDirectory` with allocation-free `FileError` translation.
- [x] 5.5 Keep `OsFileSystem` separate from portable actors and add lexical provider-replacement tests using a pure user-defined `FileSystem`.

## 6. Acceptance and Handoff

- [x] 6.1 Add native acceptance fixtures for root paths, nested paths, missing entries, wrong kinds, permissions, oversized names, partial I/O, close failures, and failed-write unspecified contents.
- [x] 6.2 Add security fixtures proving symlink traversal and every root-escape form are rejected without touching the outside target.
- [x] 6.3 Add tooling tests proving provider code navigates to canonical Silk source and only low-level calls resolve to `Intrinsic`.
- [x] 6.4 Regenerate committed manifests and goldens, run `pnpm check`, and run `pnpm release:candidate` because standard-library and runtime package contents change.
