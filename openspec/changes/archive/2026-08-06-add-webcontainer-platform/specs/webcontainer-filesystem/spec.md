## Purpose

Provides Effect programs with the standard `FileSystem` service backed by WebContainer's virtual filesystem while making compatibility limitations explicit and typed.

## ADDED Requirements

### Requirement: Standard filesystem service provisioning
The package SHALL provide a layer that derives Effect's standard `FileSystem` service from an acquired WebContainer runtime, allowing consumers to remain independent of the WebContainer API.

#### Scenario: Existing filesystem consumer uses WebContainer storage
- **WHEN** an Effect program requiring `FileSystem` is provided the WebContainer filesystem layer
- **THEN** its filesystem operations target the acquired WebContainer virtual filesystem without requiring a WebContainer-specific service

### Requirement: Native filesystem operations
The filesystem service SHALL faithfully support directory creation, directory reading, byte and string file reads, byte and string file writes, rename, removal, and existence or access checks using WebContainer path semantics.

#### Scenario: Write and read bytes
- **WHEN** a consumer writes bytes to a valid path and reads that path
- **THEN** the read returns the same byte content

#### Scenario: Rename and remove an entry
- **WHEN** a consumer renames an existing entry and then removes the renamed entry
- **THEN** the original path no longer exists and the renamed path is removed

#### Scenario: Recursively create directories
- **WHEN** a consumer creates a nested directory with recursive creation enabled
- **THEN** all missing directory segments are created

### Requirement: Derived filesystem operations
The filesystem service SHALL derive recursive directory listing, file and directory copy, temporary file and directory creation, lexical real-path resolution, truncation, readable streams, and writable sinks from the WebContainer primitives where their observable semantics can be preserved.

#### Scenario: Recursively copy a directory
- **WHEN** a consumer copies a directory tree to a valid destination
- **THEN** the destination contains the same directory structure and file bytes

#### Scenario: Temporary entry closes with its scope
- **WHEN** a consumer creates a scoped temporary file or directory and the scope closes
- **THEN** the temporary entry is removed even after failure or interruption

#### Scenario: Stream a file
- **WHEN** a consumer streams an existing file with an offset or byte limit
- **THEN** the stream emits the corresponding byte range and then ends

#### Scenario: Write through a sink
- **WHEN** a consumer runs the filesystem sink with multiple byte chunks
- **THEN** the destination file contains those chunks in input order

### Requirement: Explicit stat approximation
The filesystem service SHALL return accurate file-versus-directory type and file byte size when stat information can be derived. Metadata that WebContainer does not expose SHALL be represented with documented stable neutral values or absence rather than invented host metadata.

#### Scenario: Stat a file
- **WHEN** a consumer stats an existing file
- **THEN** the result identifies a file, reports its current byte size, and marks unavailable timestamps and identifiers as absent

#### Scenario: Stat a directory
- **WHEN** a consumer stats an existing directory
- **THEN** the result identifies a directory and uses the documented neutral values for unavailable size and mode metadata

### Requirement: Unsupported operations fail explicitly
Operations whose semantics cannot be upheld by WebContainer SHALL fail in the typed `PlatformError` channel with the operation and path and a description stating that the capability is unsupported. Unsupported operations SHALL include permission and ownership changes, hard and symbolic links, link reads, timestamp changes, file watching, globbing, and random-access open file handles unless a later implementation can preserve their contracts.

#### Scenario: Request an unsupported operation
- **WHEN** a consumer requests an unsupported filesystem operation
- **THEN** the operation fails with a typed `PlatformError` that names the operation and does not report the target as merely missing

#### Scenario: Watch is unsupported
- **WHEN** a consumer starts a filesystem watch stream
- **THEN** the stream fails with a typed unsupported `PlatformError` instead of polling silently or never producing events

### Requirement: Filesystem error normalization
Filesystem failures SHALL be normalized into Effect `PlatformError` reasons, preserving recognizable not-found, already-exists, permission, invalid-data, busy, and unknown failures when the underlying error supplies sufficient evidence.

#### Scenario: Missing file
- **WHEN** a consumer reads a path that does not exist
- **THEN** the operation fails with a `PlatformError` whose reason is `NotFound`

#### Scenario: Unrecognized WebContainer failure
- **WHEN** a filesystem operation rejects with a failure that cannot be classified safely
- **THEN** the operation fails with an `Unknown` `PlatformError` preserving the original failure as diagnostic ancestry

### Requirement: Filesystem compatibility documentation
The published package SHALL document every `FileSystem` operation as native, derived, approximated, or unsupported and SHALL describe any buffering, metadata, path, or concurrency limitations.

#### Scenario: Consumer evaluates compatibility
- **WHEN** a consumer reads the package filesystem documentation
- **THEN** the consumer can determine whether each standard `FileSystem` operation is native, derived, approximated, or unsupported before using the layer
