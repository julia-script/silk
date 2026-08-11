## 1. Prerequisites and Portable Path

- [x] 1.1 Confirm `add-returned-lexical-borrows` and `add-owned-bytes` are complete before implementation begins.
- [x] 1.2 Add canonical owned `Path` source with absolute-only `make`, `root`, normalization, and rejection of invalid UTF-8, NUL, empty components, `.`, and `..`.
- [x] 1.3 Implement explicit-base `resolve` with lexical dot processing and root-escape rejection plus strict normalized-relative `join`.
- [x] 1.4 Implement `asBytes`, `isRoot`, lexical borrowed `name`, and allocated owned `parent` without adding `PathSlice`.
- [x] 1.5 Add accepted and rejected Path fixtures covering root, nested paths, relative resolution, normalization, escape, and absence of ambient cwd.

## 2. Portable Values and Errors

- [x] 2.1 Add canonical `FileInfo`, `DirectoryInfo`, entry kind, and `DirectoryEntry` with owned complete child Paths.
- [x] 2.2 Add closed `FileOperation` and portable `FileReason` values covering every primitive and specified recovery reason.
- [x] 2.3 Add allocation-free `FileError` containing operation, reason, and optional numeric provider code with no Path, text, or owned diagnostic data.
- [x] 2.4 Add ownership, Drop, layout, and allocation-failure tests for Path, directory entries, metadata, and errors across evaluation and both backends.

## 3. Seven-Operation FileSystem Service

- [x] 3.1 Add the source-defined `FileSystem` runtime service with exact `readFile`, `writeFile`, `stat`, `listDirectory`, `createDirectory`, `removeFile`, and `removeDirectory` signatures.
- [x] 3.2 Verify every operation retains `&mut FileSystem`, exact `FileError`/`OutOfMemory` rows, and `&mut Allocator` only for owned result allocation.
- [x] 3.3 Add conformance and lexical provision fixtures for mutable recording, deterministic failure injection, and replacement by an application-defined provider.
- [x] 3.4 Verify no actor name receives special semantic, HIR, MIR, evaluation, or backend handling and missing provision never selects ambient storage.

## 4. Primitive Semantics and Helpers

- [x] 4.1 Add contract fixtures for complete reads, create-or-truncate writes, chunked provider writes, successful later reads, and unspecified destination contents after failure.
- [x] 4.2 Add stat and listing fixtures for file length, directory identity, owned full child paths, deterministic byte ordering, wrong kinds, and unsupported entries.
- [x] 4.3 Add create-one-directory, remove-file, and empty-only remove-directory fixtures including NotFound, AlreadyExists, WrongType, and NotEmpty.
- [x] 4.4 Implement `createDirectoriesRecursively` in ordinary source with explicit parent allocation, allocator requirement, and no recursive service primitive.
- [x] 4.5 Implement `writeFileWithParents` and `exists` in ordinary source, ensuring `exists` returns false only for NotFound and propagates every other failure.

## 5. Standard Library, Tooling, and Target Parity

- [x] 5.1 Add portable actors and helpers to the deterministic canonical standard-library manifest without importing any platform provider or ABI.
- [x] 5.2 Add hover, completion, occurrences, presentation, and go-to-definition tests for every portable value, primitive, helper, failure, and requirement row.
- [x] 5.3 Add evaluator, native LLVM, and direct-Wasm parity fixtures using pure ordinary-source user providers.
- [x] 5.4 Add artifact tests proving programs with no filesystem use and direct-Wasm programs with pure providers emit no OS filesystem imports or runtime symbols.
- [x] 5.5 Add architecture tests proving there are no FileSystem-shaped HIR/MIR operations, compiler-known actor names, platform types, hosted-Wasm ABI, or built-in providers in this change.

## 6. Documentation and Verification

- [x] 6.1 Add one readable example showing explicit Path resolution, service provision, complete read/write, helpers, and portable failure recovery with a user provider.
- [x] 6.2 Update public Path, FileSystem, service, allocation, failed-write, and provider-authoring documentation while linking platform implementation to its separate proposal.
- [x] 6.3 Regenerate committed manifests and goldens, run `pnpm check`, and run `pnpm release:candidate` because standard-library package contents change.
