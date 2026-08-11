## 1. Owned Bytes Foundation

- [ ] 1.1 Add canonical Bytes as a nominal ordinary Silk owner over Vector<u8> with construction, length, shared/exclusive slices, move, and Drop behavior
- [ ] 1.2 Add Bytes ownership, allocation-failure, evaluator, native, direct-Wasm, and standard-library manifest tests without compiler collection privilege
- [ ] 1.3 Add Bytes presentation, documentation, hover, completion, occurrences, and go-to-definition coverage

## 2. Portable Path Model

- [ ] 2.1 Add canonical Path and PathSlice values with provider-rooted UTF-8 normalization and explicit root semantics
- [ ] 2.2 Implement checked construction, relative-fragment joining, parent, name, extension, and component operations
- [ ] 2.3 Add rejection and determinism coverage for NUL, empty components, `.`, `..`, separator normalization, and hidden-current-directory assumptions
- [ ] 2.4 Add a distinct PlatformPath type and prohibit implicit interchange with portable Path

## 3. FileSystem Contracts and Values

- [ ] 3.1 Add DirectoryEntry, portable entry kind and inspection values, FileOperation, FileReason, and FileError canonical Silk declarations
- [ ] 3.2 Add the FileSystem capability for readFile, replace/create-new writes, inspect, sorted listing, directory creation, rename, and non-recursive removals
- [ ] 3.3 Verify explicit FileSystem requirement/provision, portable error rows, allocation failure separation, and no FileSystem-specific HIR or MIR operation
- [ ] 3.4 Add all new modules to the canonical standard-library manifest and generated embedded-source verification

## 4. Deterministic In-Memory Provider

- [ ] 4.1 Implement an ordinary Silk in-memory tree provider with complete owned Bytes and canonical Path keys
- [ ] 4.2 Implement atomic replace/create-new writes, file/directory kind checks, sorted listings, rename, and non-recursive removal semantics
- [ ] 4.3 Add configurable operation failures and verify portable FileError reasons, unchanged destinations, event ordering, and cleanup
- [ ] 4.4 Prove evaluator and direct-Wasm execution with the pure Silk provider and no filesystem host imports

## 5. Lower-Level Platform Boundary

- [ ] 5.1 Define the narrow PlatformFileSystem service and provider-only operations required by the native portable adapter
- [ ] 5.2 Implement the stage-0 external boundary with typed wrapping for host paths, complete reads, atomic writes, directories, rename, and removal
- [ ] 5.3 Implement a native FileSystem provider rooted at one explicit host directory and reject traversal or indirection that escapes it
- [ ] 5.4 Translate native outcomes to portable FileError reasons while retaining native codes only as diagnostic detail

## 6. Hosted WebAssembly Provider

- [ ] 6.1 Define a private versioned complete-operation Wasm import ABI with caller-owned buffers and numeric statuses
- [ ] 6.2 Implement a hosted provider adapter that maps the private ABI to ordinary FileSystem values and errors
- [ ] 6.3 Add a browser-virtual fixture host and prove parity with the in-memory and native provider trees without Unix handles or path semantics

## 7. Tooling, Labs, and Architecture Gates

- [ ] 7.1 Add completion, hover, occurrences, go-to-definition, and documentation tests for Bytes, Path, FileSystem, and PlatformFileSystem
- [ ] 7.2 Add facade-only Labs presets for path validation, whole-file success/failure, requirement rows, Bytes cleanup, provider observations, and backend artifacts
- [ ] 7.3 Add architecture tests proving portable standard-library modules never depend on PlatformFileSystem and backends contain no filesystem-named MIR operations
- [ ] 7.4 Add fresh-process determinism for standard-library imports, directory ordering, errors, MIR, native artifacts, and direct-Wasm bytes

## 8. Acceptance and Documentation

- [ ] 8.1 Add one readable example that runs unchanged against native, in-memory, and hosted-Wasm virtual providers
- [ ] 8.2 Update public standard-library, path, service, browser-hosting, and platform-escape-hatch documentation
- [ ] 8.3 Run `pnpm typecheck`, `pnpm exec biome check .`, focused filesystem tests, `pnpm test`, `pnpm check`, and `pnpm release:candidate`
