## Why

Between deterministic bitcode and a runnable binary sit three pinned pieces of orchestration: the
pinned external Clang process that optimizes bitcode into a target object, the named build scope
that owns intermediate artifacts, and the `NativeLinker` service whose bootstrap implementation
drives the Clang link driver. None of it is linking algorithms — it is process orchestration with
provenance, plus the minimal C runtime shim the slice needs to reach a native entry.

## What Changes

- Complete the `Backend` object contract: invoke a pinned external Clang with `-c` and structured
  arguments to optimize bitcode and emit the target object. Optimization profiles are fixed:
  debug `-O0` with debug metadata, release `-O2` stripping debug metadata, release-with-debug
  `-O2` with line information. No configurable pass pipeline; correctness never depends on LLVM
  optimizing successfully.
- Bitcode and object intermediates become owned, path-backed artifacts tied to a named build
  scope, removed at scope exit on success or failure; retention is an explicit `save temps`-style
  promotion. Large Clang outputs are not read into memory merely to be written again.
- Add the `NativeLinker` service and its `ClangLinker` implementation: validate target-compatible
  inputs, combine the program object with selected runtime objects and approved system libraries,
  invoke the pinned Clang driver with structured arguments (never a shell string), retain process
  output, status, and command provenance on failure, and write the executable to the requested
  durable destination.
- Add the minimal C runtime shim for the slice, compiled by the pinned toolchain: a private,
  compiler-versioned scalar ABI reaching a closed native entry — not user-facing FFI, per issue 07.
- Add the inspector lab: toolchain provenance — commands issued, artifact sizes, process status,
  and build-scope lifetimes.

## Capabilities

### New Capabilities

- `bootstrap-native-toolchain`: Pinned-Clang orchestration for object emission, build-scope-owned
  intermediates, the `NativeLinker`/`ClangLinker` service, and the minimal runtime shim slice.

### Modified Capabilities

- `bootstrap-backend`: The `Backend` service now fulfills its relocatable-object contract.
- `bootstrap-syntax-inspector`: Toolchain-provenance lab.

## Impact

Introduces external process dependencies (pinned Clang) and filesystem-scoped artifacts to the
compiler; the driver orchestrates backend and linker calls itself — no Node.js or TypeScript
harness may perform a stage required for stage-2 self-hosting.

## Plan References

- [Roadmap — Track 5, proposal 12](../../../roadmaps/compiler-realignment.md)
- [Issue 06](../../../wayfinder/bootstrap-language/issues/06-bootstrap-compiler-pipeline.md):
  the `LlvmBackend`/Clang paragraph ("invokes a pinned external Clang process with `-c`"), the
  build-scope paragraph ("Bitcode and object intermediates are owned, path-backed artifacts tied
  to a named build scope. Leaving that scope removes them after success or failure."), the
  `NativeLinker` paragraph ("invokes the pinned Clang driver with structured arguments rather
  than a shell command string"), the runtime-shim paragraph ("a deliberately small C runtime shim
  … private, compiler-versioned scalar ABI"), and the optimization-profiles paragraph.
- [Issue 07 — Minimum runtime and standard library](../../../wayfinder/bootstrap-language/issues/07-minimum-runtime-and-standard-library.md):
  owns the exact runtime capabilities and shim ABI.
