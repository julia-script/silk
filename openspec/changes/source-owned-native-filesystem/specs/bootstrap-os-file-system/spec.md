## MODIFIED Requirements

### Requirement: OS resources use one opaque affine handle representation

Native filesystem resources SHALL use ordinary affine source representations. Every successful open transfers one close obligation. The compiler SHALL NOT expose or recognize OsHandle, resource constructors or filesystem handle access operations.

#### Scenario: Move an open handle into cleanup

- **WHEN** a source wrapper successfully opens a file and passes the handle to consuming close
- **THEN** the original binding becomes unavailable and the resource receives exactly one close attempt

#### Scenario: Reject copying a handle

- **WHEN** source attempts to copy or duplicate an affine native resource
- **THEN** ownership rejects the operation before emission

### Requirement: OS intrinsics report low-level outcomes without library values

Ordinary selected extern calls SHALL return their exact native outcomes. Source SHALL validate counts, capture native errors and construct portable values. The compiler MUST NOT recognize Option, Path, Bytes, DirectoryEntry, FileError or FileSystem by spelling. Read and write can complete partially; source owns completion and translation.

#### Scenario: Report a failed open

- **WHEN** the host refuses a file open
- **THEN** source captures the native error before cleanup and returns FileError without creating a resource owner

#### Scenario: Transfer a successful open

- **WHEN** a file or directory open succeeds
- **THEN** ordinary source creates one affine owner and transfers one close obligation

#### Scenario: Report a partial write

- **WHEN** the host accepts fewer bytes than the supplied slice
- **THEN** ordinary source validates the exact positive byte count and continues or translates a later failure

### Requirement: Directory iteration is retryable and deterministic at the protocol boundary

Directory-next SHALL distinguish entries, EOF, failure and insufficient capacity in ordinary source. An insufficient name buffer SHALL report required capacity and retain an owned pending entry without another readdir. Source SHALL construct and sort portable results.

#### Scenario: Retry an oversized directory name

- **WHEN** the next entry does not fit the supplied buffer
- **THEN** the call reports the required capacity, leaves the iterator on the same entry, and a sufficiently sized retry returns that entry

#### Scenario: Reach directory end

- **WHEN** every host entry has been consumed
- **THEN** source reports EOF without fabricating an empty-name entry

### Requirement: OsFileSystem confines every operation beneath its native root

OsFileSystem SHALL own a copied absolute native byte root supplied without constructor I/O. Traversal SHALL preserve non-UTF-8 bytes and reject NUL, malformed components, dot, dot-dot and symlinks below the configured root. It SHALL open the root per operation and use descriptor-relative no-follow traversal. The root and its ancestors are trusted configuration; hostile cross-boundary renames and mount changes are outside this confinement guarantee.

#### Scenario: Resolve a portable root path

- **WHEN** the provider receives portable path `/` under native root `/srv/app`
- **THEN** it operates on the configured native root without treating the portable path as host root

#### Scenario: Reject symlink traversal

- **WHEN** any traversed component beneath the configured root is a symlink
- **THEN** the operation fails with the portable reason selected by ordinary source and never accesses the symlink target

### Requirement: OsFileSystem brackets fallible handles explicitly

For every acquired resource, source SHALL attempt consuming cleanup on success, typed failure and structured cancellation. Primary failure MUST survive cleanup failure; cleanup failure after otherwise successful work SHALL become the result. Source Drop SHALL provide disarmed exact-once cleanup for structured unwinding. Explicit close MUST NOT retry, including EINTR. Fatal traps carry no cleanup guarantee.

#### Scenario: Preserve a read failure over close failure

- **WHEN** a read fails and the subsequent close attempt also fails
- **THEN** the provider returns the translated read failure and still records that close was attempted

#### Scenario: Return a close failure after successful work

- **WHEN** all requested bytes are read successfully but consuming close fails
- **THEN** the provider returns the translated close failure instead of successful bytes

### Requirement: OsFileSystem implements whole-file portable semantics

The provider SHALL implement the portable seven-operation service with ordinary source calls. readFile SHALL accumulate complete owned bytes. writeFile SHALL create or truncate only a checked regular file and complete partial writes. listDirectory SHALL own full byte paths and sort by bytes. Failure can leave a write destination changed, without rollback or atomic replacement.

#### Scenario: Read a complete file through partial host reads

- **WHEN** one or more host reads return fewer bytes than requested before end of file
- **THEN** the provider accumulates every chunk and returns the complete ordered bytes

#### Scenario: List entries in portable order

- **WHEN** the host enumerates child names in arbitrary order
- **THEN** the provider constructs owned full child paths and returns entries sorted by portable path bytes

#### Scenario: Leave failed writes unspecified

- **WHEN** a write fails after the host has accepted an earlier chunk
- **THEN** the provider reports `FileError` and callers make no assumption about the destination's resulting contents

### Requirement: OS support is native-only

Native filesystem members SHALL be selected only on the supported native target/libc pairs and absent on Wasm and no-libc profiles. Compiler core modules MUST NOT import host filesystem APIs merely because OsFileSystem source is packaged.

#### Scenario: Keep portable Wasm clean

- **WHEN** a LLVM-generated WebAssembly program either uses no filesystem or provides its own pure `FileSystem`
- **THEN** the emitted module contains no OS filesystem imports and does not require `OsFileSystem`

#### Scenario: Reject reachable OsFileSystem on LLVM-generated WebAssembly

- **WHEN** source requests an OsFileSystem member for LLVM-generated WebAssembly
- **THEN** ordinary selected-source availability reports the member as unavailable before emission
