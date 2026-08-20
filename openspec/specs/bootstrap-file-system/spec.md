# bootstrap-file-system Specification

## Purpose

Define a portable explicit whole-file service and provider-rooted path model that applications can
implement without depending on operating-system filesystem mechanisms.

## Requirements

### Requirement: Path is provider-absolute and normalized

`Path` SHALL be a nominal owned normalized UTF-8 path whose leading `/` denotes the root of the
selected provider, not the host operating system. `Path.make` SHALL accept only absolute input and
reject NUL, invalid UTF-8, empty interior components, `.`, `..`, and missing leading `/`.
`Path.root` SHALL construct `/`. Path construction and use MUST NOT consult an ambient current
directory or host path syntax.

#### Scenario: Construct a provider-absolute path

- **WHEN** source constructs `/project/src/Main.silk`
- **THEN** every provider receives the same normalized components relative to its own root

#### Scenario: Reject a relative Path construction

- **WHEN** `Path.make` receives `src/Main.silk`
- **THEN** it returns invalid-path failure before any FileSystem provider is invoked

#### Scenario: Reject traversal in an owned Path

- **WHEN** `Path.make` receives an absolute spelling containing `.` or `..`
- **THEN** it rejects the spelling rather than preserving a traversal-bearing owned value

### Requirement: Relative input is resolved immediately against an explicit base

`Path.resolve(base, relative)` SHALL process `.` and `..` lexically against one explicit absolute
base and return a new normalized owned `Path`. Resolution MUST fail if `..` would escape the provider
root. `Path.join(base, fragment)` SHALL be the stricter operation for an already normalized non-empty
relative fragment and MUST reject absolute input, `.`, `..`, NUL, invalid UTF-8, and empty
components. The first cut SHALL NOT expose an owned `RelativePath` or unresolved path value.

#### Scenario: Resolve dot components with an explicit base

- **WHEN** source resolves `../assets/./logo.bin` against `/project/src`
- **THEN** it receives owned path `/project/assets/logo.bin` without consulting process state

#### Scenario: Reject root escape

- **WHEN** source resolves `../../outside` against `/project`
- **THEN** resolution fails because the second parent step would leave the provider namespace

#### Scenario: Join a normalized child

- **WHEN** source joins `/project` with `src/Main.silk`
- **THEN** it receives `/project/src/Main.silk`

### Requirement: Path exposes only minimal owned and borrowed operations

The first Path API SHALL include `make`, `root`, `join`, `resolve`, `asBytes`, `isRoot`, `name`, and
`parent`. `asBytes` and a non-root `name` SHALL return shared lexical byte views tied to the Path
owner without allocating. Because the conservative returned-borrow subset cannot place a borrow
inside `Option`, `name` SHALL return an empty lexical view for root; empty non-root components are
invalid, and `isRoot` distinguishes absence. `parent` SHALL return an allocated owned `Option<Path>`,
with `None` for root. The API MUST NOT introduce `PathSlice` or store a borrow inside another value.

#### Scenario: Borrow a path name

- **WHEN** source asks for the name of `/project/Main.silk`
- **THEN** it receives a shared lexical view of `Main.silk` tied to the original Path

#### Scenario: Own a parent independently

- **WHEN** source asks for the parent of `/project/Main.silk`
- **THEN** it receives owned `/project` through the ordinary allocation contract and may later move the original Path

#### Scenario: Inspect root

- **WHEN** source calls `isRoot`, `name`, and `parent` on `Path.root`
- **THEN** it observes `true`, an empty lexical name view, and `None` respectively

### Requirement: FileSystem has seven mutable whole-file primitives

`FileSystem` SHALL be a source-defined runtime service with exactly these primitive operation
contracts:

```silk
readFile(&Path)
  -> Bytes ! FileError | OutOfMemoryError ? &mut FileSystem | &mut Allocator
writeFile(&Path, &[u8])
  -> () ! FileError ? &mut FileSystem
stat(&Path)
  -> FileInfo | DirectoryInfo ! FileError ? &mut FileSystem
listDirectory(&Path)
  -> Vector<DirectoryEntry> ! FileError | OutOfMemoryError
     ? &mut FileSystem | &mut Allocator
createDirectory(&Path)
  -> () ! FileError ? &mut FileSystem
removeFile(&Path)
  -> () ! FileError ? &mut FileSystem
removeDirectory(&Path)
  -> () ! FileError ? &mut FileSystem
```

Every operation SHALL use `&mut FileSystem`, including observations, so a conforming provider may
record calls, update caches, or inject deterministic failures. Missing provision MUST NOT select an
ambient OS filesystem.

#### Scenario: Provide a user filesystem lexically

- **WHEN** a program supplies an ordinary user-defined `FileSystem` service implementation
- **THEN** every primitive dispatches to that provider with the declared failure and requirement rows

#### Scenario: Reject a missing provider

- **WHEN** a closed entry retains `&mut FileSystem` without provision
- **THEN** analysis or execution reports the unsatisfied service requirement without touching host storage

#### Scenario: Record an observational call

- **WHEN** a mutable test provider records a successful `stat` request
- **THEN** the call is valid even though its portable result does not otherwise mutate filesystem contents

### Requirement: Whole-file operations do not specify physical atomicity

`writeFile` SHALL receive one complete immutable byte view through one service operation, create a
missing file, and truncate an existing file. A provider MAY perform any number of physical writes.
After success, a later ordered read SHALL observe the complete supplied bytes. After failure, the
destination contents SHALL be unspecified. The portable contract MUST NOT require transactional
replacement, rollback, previous-content preservation, or one physical write and MUST NOT expose
streaming or public file handles.

#### Scenario: Implement a write in chunks

- **WHEN** a provider writes one supplied byte view through several lower-level operations and succeeds
- **THEN** the caller observes one successful service operation and a later read returns the complete supplied bytes

#### Scenario: Fail after partial physical progress

- **WHEN** a provider fails after changing some destination bytes
- **THEN** it returns `FileError` and portable callers make no assumption about the resulting contents

#### Scenario: Create or truncate

- **WHEN** `writeFile` targets a missing path and later an existing file
- **THEN** the first call creates it and the second replaces its visible contents after success

### Requirement: Portable metadata and directory entries are small owned values

`stat` SHALL return `FileInfo { byteLength: usize }` for a file or `DirectoryInfo` for a directory.
`DirectoryEntry` SHALL own the complete normalized child `Path` and its file-or-directory kind.
`listDirectory` SHALL return only immediate children sorted by complete portable path bytes,
independent of provider enumeration order. The portable API MUST NOT expose links, inode identity,
permissions, timestamps, ownership, devices, or host-native metadata.

#### Scenario: Stat a file

- **WHEN** a provider stats a regular file containing 42 bytes
- **THEN** it returns `FileInfo` with `byteLength` 42

#### Scenario: List owned full paths deterministically

- **WHEN** a provider enumerates `/project/b` before `/project/a`
- **THEN** `listDirectory(/project)` returns owned entries for `/project/a` then `/project/b`

#### Scenario: Reject an unrepresentable entry

- **WHEN** a provider encounters an entry that is neither a portable file nor directory
- **THEN** the operation fails with `Unsupported` rather than inventing metadata

### Requirement: FileError is allocation-free and portable

`FileError` SHALL contain the `FileOperation`, one closed portable `FileReason`, and an optional
numeric provider/native code. It MUST NOT own or borrow a `Path`, text message, provider object, or
other allocation. Reasons SHALL include `NotFound`, `AlreadyExists`, `PermissionDenied`,
`InvalidPath`, `WrongType`, `NotEmpty`, `NoSpace`, `TooLarge`, `Unsupported`, and `Other`.
Allocation exhaustion SHALL remain the separate `OutOfMemoryError` failure.

#### Scenario: Translate a missing entry

- **WHEN** a provider cannot find the requested path
- **THEN** it returns `FileError` naming the attempted operation and `NotFound`, optionally retaining a numeric provider code

#### Scenario: Keep allocation failure separate

- **WHEN** `readFile` obtains file bytes but cannot allocate the owned `Bytes` result
- **THEN** it fails with `OutOfMemoryError` rather than wrapping allocation exhaustion in `FileError`

### Requirement: Recursive and convenience behavior is ordinary source composition

Canonical source SHALL define `createDirectoriesRecursively`, `writeFileWithParents`, and `exists`
as ordinary functions above the seven service primitives. Recursive parent creation and owned parent
paths SHALL retain `OutOfMemoryError ? &mut Allocator` in addition to `&mut FileSystem` and `FileError`.
`exists` SHALL return `false` only for `NotFound` and MUST propagate every other failure. The service
MUST NOT add recursive creation, recursive removal, or parent-writing primitives.

#### Scenario: Create missing parents recursively

- **WHEN** `createDirectoriesRecursively` receives `/a/b/c` and only `/a` exists as a directory
- **THEN** it creates `/a/b` and `/a/b/c` using ordinary `stat` and `createDirectory` calls

#### Scenario: Write with missing parents

- **WHEN** `writeFileWithParents` receives a file whose parent directories are absent
- **THEN** it creates the parents recursively and then delegates the complete bytes to `writeFile`

#### Scenario: Propagate an exists permission failure

- **WHEN** `exists` calls `stat` and receives `PermissionDenied`
- **THEN** it propagates that `FileError` rather than returning `false`

#### Scenario: Remove only an empty directory

- **WHEN** `removeDirectory` targets a non-empty directory
- **THEN** the primitive returns `NotEmpty` and performs no recursive removal

### Requirement: Portable filesystem use remains provider- and target-neutral

Equivalent programs supplied with conforming providers SHALL preserve service call order, success
values, portable failures, byte ownership, directory ordering, and provider mutations through MIR
evaluation, native LLVM execution, and direct Wasm. A program that does not use `FileSystem`, and a
direct-Wasm program supplied with a pure user-defined implementation, MUST NOT require OS filesystem
imports or runtime support.

#### Scenario: Run a pure provider on direct Wasm

- **WHEN** a direct-Wasm program supplies its own ordinary `FileSystem` and reaches no target-specific intrinsic
- **THEN** it executes the portable service with no OS filesystem imports

#### Scenario: Emit a program with no filesystem use

- **WHEN** executable closure contains no FileSystem operations or OS filesystem intrinsics
- **THEN** its artifact contains no filesystem host import or runtime symbol

#### Scenario: Compare conforming providers

- **WHEN** two providers expose the same logical tree and receive the same ordered portable operations
- **THEN** callers observe the same bytes, metadata, entry ordering, and portable reasons

### Requirement: A Path admits exact platform bytes

`Path` SHALL offer construction from exact platform bytes and a borrowed view of those bytes. Both
SHALL apply every normalization rule textual construction applies — absolute, NUL-rejecting, and
rejecting `.`, `..`, empty components, and trailing separators — and SHALL lift only the requirement
that the value be well-formed text. The `string` construction and view operations SHALL keep their
existing checked meaning, and this capability MUST NOT introduce a second text or path type.

#### Scenario: Round-trip a platform path that is not text

- **WHEN** a Path is built from bytes that are not well-formed text
- **THEN** the byte view returns those exact bytes unchanged

#### Scenario: Keep normalization for byte construction

- **WHEN** byte construction receives a relative path, a NUL, or an unnormalized component
- **THEN** it is rejected exactly as the textual constructor rejects it
