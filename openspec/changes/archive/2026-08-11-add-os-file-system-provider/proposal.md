## Why

The portable FileSystem contract needs one useful implementation for native applications without
making operating-system handles or path rules part of the portable API. The compiler should expose
only the unsafe platform primitives from which ordinary Silk can build that provider.

## What Changes

- Add one opaque affine `OsHandle` representation and unsafe `Intrinsic` operations for confined
  file open/read/write, directory open/next, path inspection, directory creation, file/directory
  removal, and consuming handle close.
- Return low-level success values through `Option`/`bool` plus stable numeric reason and native-code
  outputs; the compiler does not construct `Path`, `Bytes`, `DirectoryEntry`, or `FileError`.
- Add ordinary source-defined `OsFileSystem`, owning a copied absolute native root and implementing
  whole-file portable operations over explicit handle brackets.
- Require explicit fallible close on every success and failure path; keep `Drop` infallible and
  defer automatic fallible cleanup syntax.
- Enforce per-operation root confinement, reject symlink traversal and namespace escape, and keep
  physical write strategy provider-owned with no rollback guarantee.
- Inject the evaluator OS host at an outer platform boundary and add reachable-only native LLVM
  runtime symbols.
- Reject reachable OS intrinsics on direct Wasm while allowing no-filesystem and user-provided
  FileSystem programs to emit no OS imports.
- Require `add-portable-file-system` and `add-target-restricted-intrinsics` before implementation.

## Capabilities

### New Capabilities

- `bootstrap-os-file-system`: Unsafe OS handle primitives, confined native provider semantics,
  explicit resource cleanup, error translation, and target behavior.

### Modified Capabilities

- `bootstrap-intrinsic-boundary`: Admit the smallest platform handle and path operations needed by
  the ordinary source provider, all under `Intrinsic` with explicit unsafe invariants.
- `bootstrap-evaluation`: Accept an injected OS host without importing Node filesystem APIs into
  browser-capable compiler cores.
- `bootstrap-backend`: Emit native OS runtime operations only when reachable and reject them on
  unsupported targets through generic target availability.
- `bootstrap-silk-stdlib`: Ship `OsFileSystem` separately from the portable FileSystem module and
  keep native mechanisms out of portable signatures.

## Impact

The change affects the intrinsic inventory, opaque value/layout handling, ownership of affine
handles, evaluator host configuration, native runtime shims and linking, standard-library source,
typed error translation, acceptance fixtures, tooling, and documentation. It adds no public file
handle API, `PlatformFileSystem` service, hosted-Wasm filesystem ABI, built-in virtual filesystem,
implicit current directory, or effectful Drop hook.
