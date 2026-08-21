## ADDED Requirements

### Requirement: Native toolchain failures yield typed errors

Every expected native-toolchain failure (spawn, write, rename, temp-dir creation) SHALL surface in the typed error channel with operation and stage provenance and a preserved cause. An fs or subprocess failure SHALL NOT escape an Effect generator as a defect.

#### Scenario: A spawn failure is a typed failure

- **WHEN** the pinned Clang exits with a non-zero status or cannot be spawned
- **THEN** the operation yields a typed toolchain failure carrying the command and stage, not a thrown error

#### Scenario: A storage failure carries the failing stage

- **WHEN** an intermediate write fails
- **THEN** the yielded failure names the stage that failed and preserves the underlying cause

### Requirement: Artifact and cache commits are atomic and always clean up

Every durable commit SHALL stage to a temporary sibling and rename atomically, and SHALL remove the temporary on any failure so no stale temporary file survives.

#### Scenario: A failed rename leaves no temp sibling

- **WHEN** a staged write is followed by a failing rename
- **THEN** the temporary file is removed and the destination is left untouched

### Requirement: The compiler root barrel stays browser-safe

Importing the compiler package root SHALL NOT pull Node built-ins. Host target detection (platform/arch) SHALL be resolved through a Node-only boundary reached by deep import, not from modules re-exported by the root barrel.

#### Scenario: Importing the root barrel does not load node:os

- **WHEN** a consumer imports the compiler package root in a browser bundle
- **THEN** no Node built-in module is imported transitively

### Requirement: The driver does not read artifacts back from the filesystem

Artifact bytes produced by a finalizer SHALL be returned in-memory to the caller; the driver SHALL NOT re-read them with a synchronous filesystem call to seed the cache.

#### Scenario: Finalized bytes are returned in memory

- **WHEN** a finalizer produces an artifact
- **THEN** its bytes are available to the cache without a second read from disk
