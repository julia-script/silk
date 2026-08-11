## Context

The portable filesystem proposal deliberately defines a source-level service contract with no public
handles or platform types. Native applications still need a real provider. Following minimal
compiler privilege, the compiler supplies only irreducible OS resource operations; ordinary Silk
source owns whole-file loops, allocation, portable errors, sorting, and service conformance.

This change depends on both `add-portable-file-system` and
`add-target-restricted-intrinsics`. Direct Wasm gains no built-in filesystem implementation.

## Goals / Non-Goals

**Goals:**

- provide minimal file/directory handle primitives with explicit fallible cleanup;
- implement one root-confined native `OsFileSystem` in ordinary Silk source;
- preserve low-level evaluator/native parity and portable service semantics;
- keep unused OS support out of browser-capable compiler bundles and emitted artifacts.

**Non-Goals:**

- public portable file handles or a `PlatformFileSystem` service;
- compiler-known `Path`, `Bytes`, directory entries, or filesystem errors;
- symlink-following policy, implicit current directory, atomic writes, or rollback;
- hosted-Wasm, WASI, browser VFS, or built-in in-memory providers;
- automatic fallible cleanup syntax or effectful Drop.

## Decisions

### One opaque affine handle serves files and directories

`OsHandle` is a compiler representation comparable to `RawBuffer`: ordinary source can move it but
cannot inspect or copy it. Operations validate the expected handle kind. A generic consuming close
keeps cleanup composition uniform and kills the source handle whether close succeeds or fails.

Separate public file and directory handle types were rejected because the handles are provider
implementation machinery, not portable user concepts. The unsafe operation contract retains kind
invariants explicitly.

### Intrinsics are handle-level, not whole-file operations

The sealed surface covers file open/read/write, directory open/next, path inspection, creation,
removal, and close. Reads and writes report partial progress. Directory iteration writes one name
into caller storage and supports a non-advancing buffer-too-small retry. This is the smallest useful
surface from which source can build allocation, loops, entry construction, sorting, and errors.

A compiler whole-file intrinsic would hide allocation and provider policy and would duplicate the
portable service rather than enable it.

### Results use primitive success plus explicit status outputs

Open and directory-next use `Option`; other commands use `bool` or a successful scalar. Failure writes
a stable low-level reason and optional native `u32` code. The source provider maps those values into
allocation-free `FileError`. This keeps native errno-like detail available without granting the
compiler knowledge of standard-library unions or structs.

Conceptually, the protocol includes operations shaped like:

```silk
unsafe Intrinsic.osFileOpen(root, path, mode, &mut reason, &mut nativeCode)
  -> Effect<Option<OsHandle>>
unsafe Intrinsic.osFileRead(&mut handle, output, &mut reason, &mut nativeCode)
  -> Effect<Option<usize>>
unsafe Intrinsic.osFileWrite(&mut handle, input, &mut reason, &mut nativeCode)
  -> Effect<Option<usize>>
unsafe Intrinsic.osDirectoryNext(
  &mut handle,
  output,
  &mut kind,
  &mut requiredCapacity,
  &mut reason,
  &mut nativeCode,
) -> Effect<Option<usize>>
unsafe Intrinsic.osHandleClose(move handle, &mut reason, &mut nativeCode)
  -> Effect<bool>
```

Exact syntax will follow the canonical intrinsic declaration conventions; the semantic protocol is
fixed by the specs.

### Every operation is root-confined independently

`OsFileSystem` owns copied bytes for one absolute native root and passes that root plus a normalized
provider-absolute portable path to each open or path operation. The runtime opens/traverses the root
per call, rejects symlink components and namespace escapes, and does not keep a long-lived root
handle. This favors a simple ownership model and strict confinement over maximum syscall economy.

### Fallible cleanup is explicit source control flow

The provider brackets every acquired handle. It always attempts consuming close, preserves an
earlier operation error over close failure, and reports close failure when work otherwise succeeded.
Ordinary Drop stays infallible. A future `defer` construct may reduce boilerplate but is not required
for correct mechanics.

### The evaluator host is injected; native support is reachable-only

Compiler core depends on an adapter interface, not Node filesystem modules or process globals. Hosts
that want OS evaluation inject an implementation at the outer edge. Native LLVM links runtime
operations selected by executable intrinsic inventory. Direct Wasm rejects reachable OS intrinsics
through the generic availability mechanism and invents no ABI.

## Risks / Trade-offs

- Reopening/traversing the root for every operation costs syscalls. It avoids a long-lived handle in
  the portable provider and can be optimized later behind identical intrinsic semantics.
- Symlink rejection is stricter than many host APIs. The policy makes root confinement portable and
  auditable; following links safely would require its own design.
- Manual cleanup creates verbose source and is easy to get wrong. Exhaustive success/failure/close
  fixtures will gate the provider until a general language construct exists.
- Native reason normalization can drift by platform. One compiler-owned closed reason table and
  parity fixtures will define the boundary.

## Migration Plan

Land generic target availability first. Add the opaque representation and low-level protocol behind
inventory tests, then implement injected evaluator and native runtime adapters. Finally add canonical
`OsFileSystem` source and portable acceptance tests. No existing API compatibility is preserved in
alpha. Rollback removes the provider and reachable runtime entries without changing the portable
service contract.

## Open Questions

None for the native first cut. WASI, browser virtual filesystems, symlink policy, public streaming
handles, and `defer` remain independent future proposals.
