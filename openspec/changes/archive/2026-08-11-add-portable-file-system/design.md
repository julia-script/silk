## Context

See `proposal.md` for motivation. Silk has source-defined runtime services, typed Effect
requirements, affine allocation, `Vector<T>`, and complete borrowed slice inputs. The portable
filesystem contract additionally depends on returned lexical borrows and the separate source-defined
owned `Bytes` foundation. Platform intrinsics and the native provider are planned independently in
`add-target-restricted-intrinsics` and `add-os-file-system-provider`.

The design therefore contains no provider implementation. It fixes the ordinary source contract
against which native, Wasm-hosted, virtual, test, and application-specific providers can later be
written.

## Goals / Non-Goals

**Goals:**

- define one small whole-file service with explicit mutable provision;
- give paths provider-independent, provider-absolute meaning with explicit relative resolution;
- keep allocation, ownership, error, ordering, and failed-write behavior precise;
- build recursion and convenience as ordinary source helpers;
- preserve pure user-provider and no-use direct-Wasm emission without OS imports.

**Non-Goals:**

- implementing native, in-memory, browser, hosted-Wasm, WASI, or virtual providers;
- public handles, streams, seeking, mapping, locking, watchers, links, or recursive removal;
- platform paths, implicit cwd, String, formatting, broad metadata, permissions, or timestamps;
- atomic replacement, transactional rollback, create-new writes, rename, or backend filesystem ops.

## Decisions

### Path values are always provider-absolute

An owned `Path` begins with `/`, which identifies the selected provider's root rather than host root.
`Path.make` accepts only already-absolute normalized input. Relative bytes never remain as an owned
ambiguous value: `Path.resolve` consumes them immediately with an explicit absolute base, processes
`.` and `..` lexically, and rejects escape above provider root. `Path.join` is the stricter operation
for a normalized relative child.

Allowing ambient relative paths was rejected because the same Effect would depend on hidden process
or provider state. A first-class owned `RelativePath` is unnecessary for this cut and can be added
later if callers need to retain unresolved intent.

### Borrow path observations; own parent paths

`asBytes` and `name` return lexical shared views and allocate nothing. Root has an empty name view,
which is unambiguous because empty path components are invalid and avoids wrapping a borrow in
`Option` beyond the conservative returned-borrow subset. `parent` returns an owned `Option<Path>`,
which may allocate. Introducing `PathSlice` would require storing or propagating a
second lifetime-bearing domain type before there is evidence that its complexity is useful. The
owned parent keeps APIs composable under the conservative returned-borrow model.

### Seven mutable service operations form the primitive boundary

The service includes read, create-or-truncate write, stat, list, create-one-directory,
remove-file, and remove-empty-directory. Every operation requires `&mut FileSystem`, even reads, so
recording providers, deterministic failure injection, caches, and stateful virtual providers need no
exception to the contract.

Create-new, rename, recursive creation, and recursive removal were considered as primitives. The
first two can wait for demonstrated semantics; recursive creation composes from stat/create and is
provided as a helper; recursive removal is intentionally absent because its destructive scope
deserves separate design.

### Complete write input does not imply physical atomicity

The caller dispatches one service operation with the entire message-like byte view. This defines the
logical operation boundary and works for providers that cannot expose byte-by-byte user streaming.
The provider remains free to chunk, buffer, replace, or directly write. Success makes the complete
bytes visible to a later ordered read; failure leaves destination contents unspecified.

Transactional, atomic, and rollback semantics were rejected because they were accidentally carried
over from logging's message-boundary requirement. Filesystem providers and hosts cannot all promise
those properties for the minimal write operation.

### Results own data; errors remain allocation-free

`readFile` owns `Bytes`; directory listings own a Vector of full child Paths and kinds. `stat` returns
only file length or directory identity. These result allocations surface `OutOfMemory` and
`&mut Allocator` explicitly.

`FileError` stores only operation, a closed portable reason, and optional numeric provider code. It
does not retain the attempted Path or formatted text, so providers can report ordinary I/O failure
without allocating or losing the primary error to diagnostic construction.

### Convenience operations remain ordinary source

`createDirectoriesRecursively`, `writeFileWithParents`, and `exists` compose the primitives.
Recursive helpers expose their parent-Path allocation and allocator requirement; they do not broaden
base service rows. `exists` catches only `NotFound`, because permissions and other failures do not
prove absence. Optional `isFile` and `isDirectory` may be added later as equally ordinary stat
helpers without changing the service.

### The contract contains no provider or platform boundary

The canonical module exports portable values, service, and helpers only. User providers conform
through ordinary source. A separate OS proposal adds unsafe handle intrinsics and `OsFileSystem`;
future WASI or browser VFS providers can implement this same service without modifying it. Backends
therefore lower only generic service/effect/data operations.

## Risks / Trade-offs

- **[Owned full paths make listings allocate more]** → Keep entries self-contained and lifetime-free;
  later iterators or borrowed directory views require their own resource/lifetime design.
- **[No ambient relative path is less convenient]** → Make explicit-base `resolve` concise and let
  applications own a base Path in configuration.
- **[All operations require mutable provision]** → Accept the exclusive sequencing cost so stateful
  providers remain honest and deterministic.
- **[Failed writes have weak postconditions]** → Document contents as unspecified and require a
  later `stat` or read before relying on the destination.
- **[Allocation-free errors contain less context]** → Callers already possess the attempted Path;
  formatting can combine it with operation, reason, and numeric detail later.

## Migration Plan

1. Complete `add-returned-lexical-borrows` and `add-owned-bytes`.
2. Add Path and portable metadata/error actors as canonical Silk source.
3. Add the seven-operation FileSystem service and exact Effect signatures.
4. Add ordinary recursive/existence helpers and canonical manifest/tooling coverage.
5. Add pure user-provider fixtures for evaluator, native LLVM, and direct Wasm parity and pay-for-use.
6. Update public documentation and examples, then run repository and release-candidate checks.

Rollback removes the new canonical modules and fixtures. No host ABI, persisted provider format, or
compatibility adapter is introduced by this change.

## Open Questions

None. Platform providers, String formatting, richer metadata, public streaming handles, and further
helpers remain separate proposals.
