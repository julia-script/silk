# WebContainer Filesystem Delta

## ADDED Requirements

### Requirement: Filesystem watching
The filesystem service SHALL provide `watch` as a stream of standard watch events for an existing
file or directory, backed by WebContainer's native watch capability rather than polling. Each
consumer SHALL receive an independent subscription whose underlying watcher is registered when the
stream starts and closed when the stream ends, fails, or is interrupted. Events SHALL identify the
affected path and classify it as created, updated, or removed; a rename notification whose target
cannot be classified reliably SHALL be resolved using directory listings, never file contents.

#### Scenario: Watch reports a change

- **WHEN** a consumer watches a directory and a file inside it is written by another party, such
  as a spawned process
- **THEN** the stream emits a watch event identifying the affected path with a created or updated
  classification

#### Scenario: Watch reports a removal

- **WHEN** a consumer watches a directory and an entry inside it is removed
- **THEN** the stream emits a watch event identifying the removed path with a removed
  classification

#### Scenario: Watcher lifecycle follows the stream

- **WHEN** a consumer interrupts or finishes a watch stream
- **THEN** the underlying native watcher is closed and no further callbacks are delivered

#### Scenario: Watching a missing path fails

- **WHEN** a consumer watches a path that does not exist
- **THEN** the stream fails with a typed `PlatformError` whose reason is `NotFound`

## MODIFIED Requirements

### Requirement: Explicit stat approximation
The filesystem service SHALL derive stat answers from directory listings alone and SHALL NOT read
file contents to answer `stat`, `access`, or `exists`. Stat results SHALL report accurate
file-versus-directory type; file byte size and other metadata that WebContainer does not expose
SHALL be represented with documented stable neutral values or absence rather than invented host
metadata.

#### Scenario: Stat a file

- **WHEN** a consumer stats an existing file
- **THEN** the result identifies a file, uses the documented neutral value for byte size, marks
  unavailable timestamps and identifiers as absent, and the operation does not read the file's
  contents

#### Scenario: Stat a directory

- **WHEN** a consumer stats an existing directory
- **THEN** the result identifies a directory and uses the documented neutral values for
  unavailable size and mode metadata

#### Scenario: Existence checks scale with directory size

- **WHEN** a consumer checks the existence of an entry
- **THEN** the answer is derived from the parent directory listing without reading any file
  contents

### Requirement: Unsupported operations fail explicitly
Operations whose semantics cannot be upheld by WebContainer SHALL fail in the typed
`PlatformError` channel with the operation and path and a description stating that the capability
is unsupported. Unsupported operations SHALL include permission and ownership changes, hard and
symbolic links, link reads, timestamp changes, globbing, and random-access open file handles
unless a later implementation can preserve their contracts.

#### Scenario: Request an unsupported operation

- **WHEN** a consumer requests an unsupported filesystem operation
- **THEN** the operation fails with a typed `PlatformError` that names the operation and does not
  report the target as merely missing
