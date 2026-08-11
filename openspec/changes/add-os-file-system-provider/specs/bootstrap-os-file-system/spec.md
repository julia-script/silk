## Purpose

Define the minimal unsafe operating-system handle protocol and ordinary native provider that realize
the portable FileSystem contract without leaking platform mechanisms into portable source APIs.

## ADDED Requirements

### Requirement: OS resources use one opaque affine handle representation

The compiler SHALL expose an opaque move-only `OsHandle` representation for open files and
directories. It MUST NOT be constructible, copyable, inspectable, or storable by ordinary source
except through sealed `Intrinsic` operations. Every successful open SHALL transfer one explicit
close obligation to the caller.

#### Scenario: Move an open handle into cleanup

- **WHEN** a source wrapper successfully opens a file and passes the handle to consuming close
- **THEN** the original binding becomes unavailable and the resource receives exactly one close attempt

#### Scenario: Reject copying a handle

- **WHEN** source attempts to copy or duplicate an `OsHandle`
- **THEN** ownership rejects the operation before evaluation or emission

### Requirement: OS intrinsics report low-level outcomes without library values

Unsafe OS operations SHALL return success through `Option` or `bool` and SHALL write a stable
low-level numeric reason plus an optional native `u32` code to explicit output parameters on
failure. The compiler MUST NOT construct or recognize `Path`, `Bytes`, `DirectoryEntry`,
`FileError`, or the portable `FileSystem` service. Read and write SHALL report transferred byte
counts and MAY complete partially.

#### Scenario: Report a failed open

- **WHEN** the host refuses a file open
- **THEN** the intrinsic returns `None` and writes the normalized low-level reason and native code without constructing a standard-library error

#### Scenario: Report a partial write

- **WHEN** the host accepts fewer bytes than the supplied slice
- **THEN** the write intrinsic returns the exact positive byte count so ordinary source can continue or translate a later failure

### Requirement: Directory iteration is retryable and deterministic at the protocol boundary

Directory-next SHALL return `Some(n)` with `n > 0` for one entry, `Some(0)` for end of directory,
and `None` for failure. When the supplied name buffer is too small, it SHALL report the stable
buffer-too-small reason and required capacity without advancing the iterator. The intrinsic MUST NOT
sort entries or construct portable paths.

#### Scenario: Retry an oversized directory name

- **WHEN** the next entry does not fit the supplied buffer
- **THEN** the call reports the required capacity, leaves the iterator on the same entry, and a sufficiently sized retry returns that entry

#### Scenario: Reach directory end

- **WHEN** every host entry has been consumed
- **THEN** directory-next returns `Some(0)` without fabricating an empty-name entry

### Requirement: OsFileSystem confines every operation beneath its native root

Ordinary canonical source SHALL define `OsFileSystem` with an owned copy of one absolute native root.
Each portable operation SHALL combine that root with the provider-absolute `Path` through confined
host traversal. Traversal MUST reject symlinks, `.` or `..` namespace components, NUL, invalid host
encoding, and any attempt to escape the root. The provider SHALL open the root as part of each
operation and MUST NOT retain a long-lived root handle.

#### Scenario: Resolve a portable root path

- **WHEN** the provider receives portable path `/` under native root `/srv/app`
- **THEN** it operates on the configured native root without treating the portable path as host root

#### Scenario: Reject symlink traversal

- **WHEN** any traversed component beneath the configured root is a symlink
- **THEN** the operation fails with the portable reason selected by ordinary source and never accesses the symlink target

### Requirement: OsFileSystem brackets fallible handles explicitly

For every opened file or directory, `OsFileSystem` SHALL attempt consuming close on success, typed
failure, and other structured exits. If the primary operation fails, a close failure MUST NOT replace
that primary error. If the primary operation succeeds and close fails, the close failure SHALL
become the operation result. Ordinary infallible Drop MUST NOT silently perform this fallible close.

#### Scenario: Preserve a read failure over close failure

- **WHEN** a read fails and the subsequent close attempt also fails
- **THEN** the provider returns the translated read failure and still records that close was attempted

#### Scenario: Return a close failure after successful work

- **WHEN** all requested bytes are read successfully but consuming close fails
- **THEN** the provider returns the translated close failure instead of successful bytes

### Requirement: OsFileSystem implements whole-file portable semantics

The provider SHALL implement the portable seven-operation service contract using handle-level
intrinsics. `readFile` SHALL allocate and return complete owned bytes. `writeFile` SHALL create a
missing file or truncate an existing file and loop over partial writes. `listDirectory` SHALL own
full child paths and sort results by portable path bytes. Provider failure MAY leave a write
destination partially changed; no transactional rollback or atomic replacement is required.

#### Scenario: Read a complete file through partial host reads

- **WHEN** one or more host reads return fewer bytes than requested before end of file
- **THEN** the provider accumulates every chunk and returns the complete ordered bytes

#### Scenario: List entries in portable order

- **WHEN** the host enumerates child names in arbitrary order
- **THEN** the provider constructs owned full child paths and returns entries sorted by portable path bytes

#### Scenario: Leave failed writes unspecified

- **WHEN** a write fails after the host has accepted an earlier chunk
- **THEN** the provider reports `FileError` and callers make no assumption about the destination's resulting contents

### Requirement: OS support is native-only and injected at the evaluator boundary

OS intrinsics SHALL be available to evaluation only when an OS host adapter is explicitly supplied
and to supported native LLVM targets through reachable runtime support. They SHALL be unavailable to
direct Wasm. Browser-capable compiler core modules MUST NOT import Node or other operating-system
filesystem APIs merely because `OsFileSystem` source is packaged.

#### Scenario: Evaluate with an injected OS host

- **WHEN** evaluation reaches an OS intrinsic with a configured host adapter
- **THEN** the adapter performs the requested low-level operation and returns the normalized protocol result

#### Scenario: Keep portable Wasm clean

- **WHEN** a direct-Wasm program either uses no filesystem or provides its own pure `FileSystem`
- **THEN** the emitted module contains no OS filesystem imports and does not require `OsFileSystem`

#### Scenario: Reject reachable OsFileSystem on direct Wasm

- **WHEN** executable closure reaches an OS intrinsic through `OsFileSystem` for direct Wasm
- **THEN** generic target-availability validation reports the intrinsic as unavailable before emission

