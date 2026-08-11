## Context

See `proposal.md` for motivation. Silk already has affine allocation, `Vector<T>`, immutable byte
views, typed Effect requirements, user-authored capability implementations, StandardStreams host
plumbing, a TypeScript `SourceResolver`, and private native/direct-Wasm boundaries. It does not yet
have owned Bytes, owned String, a stable FFI, stored borrows, or a public platform runtime. The
portable contract must therefore own complete results and keep lower-level handles private.
The design is reconciled with the implemented distinction between runtime-provided `service`
contracts and compile-time-only `interface` conformances. `FileSystem` and `PlatformFileSystem` are
services; neither name enters the sealed `Intrinsic` catalog or MIR. Implementation begins only
after `establish-minimal-intrinsic-boundary` archives.

## Goals / Non-Goals

**Goals:**

- Establish one FileSystem contract usable unchanged on native, deterministic in-memory, and
  browser-virtual providers.
- Make portable Path, Bytes, operation, ordering, and failure semantics explicit.
- Keep native mechanisms available behind a separately named lower-level service.
- Preserve ordinary Effect provision, ownership, tooling, and evaluator/backend parity.

**Non-Goals:**

- Migrating the TypeScript stage-0 source resolver or choosing a self-hosting compiler module.
- Open handles, streaming, mapping, locking, watchers, links, permissions, timestamps, devices, or
  broad native metadata in the portable API.
- An implicit current directory, environment access, shell API, or ambient filesystem.
- An owning general-purpose String or stable public FFI/runtime ABI.

## Decisions

### 1. Portable Path is provider-rooted normalized UTF-8

`Path` owns canonical UTF-8 bytes with `/` separators. A complete service path begins at `/` inside
the configured provider namespace. Constructors reject NUL, empty interior components, `.`, and
`..`; joining accepts only normalized relative fragments. There is no current-directory lookup.

A native provider is configured with an explicit host root and maps portable components beneath it.
An in-memory or browser provider treats `/` as the root of its virtual tree. This gives identical
path meaning without pretending arbitrary Unix bytes or Windows path syntax are portable.

Alternatives considered:

- **Use host-native Path everywhere.** Preserves every OS spelling but cannot be implemented
  honestly by browser VFS providers and leaks host semantics into ordinary code.
- **Use String directly.** Avoids a type but permits unnormalized and traversal-bearing values at
  every operation.
- **Allow relative paths plus ambient cwd.** Familiar, but makes behavior depend on hidden process
  state and weakens sandboxing and determinism.

`PlatformPath` remains the lower-level value for exact host spellings. Conversion is explicit and
provider-owned.

### 2. Bytes is a nominal ordinary Silk owner

Add `Bytes` as a nominal wrapper over the existing growable `Vector<u8>` substrate. It owns arbitrary
octets, exposes immutable and exclusive slices, and follows ordinary allocator/Drop behavior.
FileSystem reads return Bytes; writes borrow bytes. This prevents Vector from becoming the permanent
public filesystem result while avoiding a compiler-known byte collection.

Bytes is useful beyond FileSystem but this slice implements only construction, slice access,
length, and ownership operations needed by whole-file I/O. Text validation and String remain
separate.

### 3. The first FileSystem surface is complete and path-based

The canonical capability contains:

- `readFile`
- `writeFileReplace`
- `writeFileCreateNew`
- `inspect`
- `listDirectory`
- `createDirectory`
- `createDirectories`
- `rename`
- `removeFile`
- `removeDirectory`

Writes are atomic at the service boundary. Directory removal is non-recursive and listings are
sorted by normalized name bytes. `inspect` returns only file/directory kind and file byte length.
Links and unrepresentable native entries return Unsupported.

This is larger than read/write alone because provider-rooted navigation, creation, and cleanup need
one coherent testable model. It remains bounded by excluding every handle- or stream-shaped API.

### 4. FileError uses portable reasons and optional provider detail

Each failure owns its operation, portable Path, semantic reason, and optional diagnostic provider
detail. Native codes are never variants in the public reason union. Allocation failure remains
separate because constructing Bytes, Path, directory entries, or retained error data can allocate.

This lets portable code recover from NotFound or AlreadyExists identically while still rendering an
errno or browser-provider detail to a human.

### 5. Providers implement semantics; backends do not recognize FileSystem

FileSystem, Path, Bytes, and provider contracts ship as ordinary Silk source. HIR and MIR contain
only existing service calls, data, ownership, and Effect execution. No backend operation is named
ReadFile or WriteFile.

The evaluator receives a provider witness. The native provider calls a lower-level platform actor
whose stage-0 implementation owns safe external-boundary wrapping and the private native shim. A
direct-Wasm hosted provider calls versioned imports with caller-owned input/output buffers and
complete operation statuses; a pure Silk in-memory provider needs no imports.

Alternatives considered:

- **Add filesystem MIR instructions.** Simplifies the first backend pass but privileges one service
  and prevents ordinary user providers.
- **Expose raw host imports as FileSystem.** Makes the implementation boundary a public language
  contract and freezes platform representations.

### 6. PlatformFileSystem is deliberately separate and initially narrow

The lower-level service owns PlatformPath and the primitives required to implement the native
portable provider. Its module is documented as host-specific and is never re-exported as the common
FileSystem API. Further handles, mapping, locks, or metadata may be added there from native program
evidence without changing portable FileSystem.

The initial implementation should expose only what the portable provider needs plus enough type
separation to prevent accidental interchange. We do not predesign a complete POSIX or Windows API.

### 7. In-memory is the semantic oracle; native and hosted Wasm are adapters

The deterministic in-memory provider owns a tree of portable paths and complete Bytes, supports a
failure ordinal, and exposes ordered observations. It drives evaluator/direct-Wasm parity without
host state. Native acceptance maps a temporary explicit host root and verifies complete operations
and error translation. Hosted-Wasm acceptance supplies equivalent imports backed by the same
logical fixture model, demonstrating browser compatibility without requiring an actual browser in
the compiler test suite.

### 8. Tooling and labs expose the portable boundary

Canonical modules enter the embedded manifest. Existing occurrence, presentation, completion,
hover, and definition machinery must navigate every portable actor and keep PlatformFileSystem
visibly distinct. Labs show Path validation, requirement rows, provider conformance, Bytes
ownership/cleanup, MIR, provider observations, and artifacts through the Analysis facade.

## Risks / Trade-offs

- **[UTF-8 Path excludes valid Unix byte paths]** → Preserve them behind PlatformPath; portable APIs
  reject what another host cannot represent honestly.
- **[Atomic replacement is expensive on some providers]** → Require providers to implement the
  semantic transaction internally or return Unsupported; never expose partial success.
- **[Bytes expands the change]** → Keep its first API minimal and implement it over existing Vector
  and allocation behavior with no compiler primitive.
- **[Native root mapping permits escape through links]** → Reject traversal components and have the
  native provider refuse indirection that escapes its configured root.
- **[Hosted-Wasm ABI leaks into public FileSystem]** → Keep imports private/versioned and test the
  same public source against pure in-memory provision.
- **[PlatformFileSystem grows into the default API]** → Keep it out of portable standard-library
  signatures and add architecture tests for dependency direction.

## Migration Plan

1. Add Bytes and Path actors with ownership, validation, formatting, and tooling coverage.
2. Add FileError, DirectoryEntry, and FileSystem contracts as canonical Silk source.
3. Implement the deterministic in-memory provider and establish semantic/evaluator acceptance.
4. Add ordinary service lowering parity and direct-Wasm execution with the pure Silk provider.
5. Add the narrow PlatformFileSystem boundary and native rooted provider with failure translation.
6. Add hosted-Wasm provider imports, browser-virtual fixture acceptance, labs, and determinism gates.
7. Update public examples to prefer FileSystem and run full repository and release-candidate checks.

Rollback removes the new standard-library modules, providers, imports, and fixtures. The language is
unreleased and no persisted filesystem format or compatibility adapter is introduced.
