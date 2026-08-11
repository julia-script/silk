## Purpose

Define a portable explicit filesystem service whose whole-file and directory semantics work across
native hosts, deterministic tests, and browser virtual file systems, with platform APIs separate.

## ADDED Requirements

### Requirement: Portable paths have provider-independent meaning

`Path` SHALL be a nominal owned normalized UTF-8 path within an explicit provider namespace. Its
canonical separator SHALL be `/`; it SHALL distinguish the provider root from relative fragments,
reject NUL, empty interior components, `.` and `..`, and never consult a process current directory.
Joining and parent/component operations MUST preserve normalization. `Path` SHALL remain distinct
from String, source-module identity, OS strings, and host-native paths.

#### Scenario: Reject traversal syntax

- **WHEN** a program attempts to construct or join a portable path containing `..`
- **THEN** Path validation returns an invalid-path result before any FileSystem provider is invoked

#### Scenario: Resolve from an explicit provider root

- **WHEN** a program reads `/project/src/Main.silk`
- **THEN** every provider interprets the same normalized components within its configured namespace without consulting a hidden current directory

### Requirement: Whole-file data uses an owned Bytes actor

The portable standard library SHALL expose nominal owned `Bytes` backed by ordinary Silk allocation
and sequence behavior. `FileSystem.readFile` SHALL return complete owned Bytes; write operations
SHALL accept an immutable byte view without requiring callers to expose the Bytes representation.
Bytes MUST NOT assert UTF-8 validity or become a filesystem-specific compiler primitive.

#### Scenario: Read arbitrary bytes

- **WHEN** a provider contains a file with invalid UTF-8 and zero bytes
- **THEN** readFile returns the exact complete byte sequence without text decoding or loss

### Requirement: FileSystem is explicit and replaceable

`FileSystem` SHALL be a nominal service capability. Every operation SHALL retain its FileSystem
requirement until supplied through ordinary Effect provision. The bootstrap portable surface SHALL
include complete-file read, replace, and create-new writes; entry inspection; lexically ordered
directory listing; single-directory and recursive-parent creation; rename; file removal; and
non-recursive empty-directory removal. Missing provision MUST NOT select an ambient OS filesystem.

#### Scenario: Replace native storage with memory

- **WHEN** the same program is provided with a native-rooted FileSystem and an in-memory FileSystem
- **THEN** its source, paths, operation results, and Effect contract remain unchanged

#### Scenario: Reject a missing provider

- **WHEN** a closed entry requires FileSystem and no provider is supplied
- **THEN** analysis or execution reports the unsatisfied requirement without touching host storage

### Requirement: Complete writes are atomic at the portable boundary

`writeFileReplace` SHALL either replace the destination with the complete supplied bytes or fail
while preserving the previous destination. `writeFileCreateNew` SHALL fail with AlreadyExists
without modifying an existing entry. A successful write SHALL become completely visible to later
operations in service order. The portable contract MUST NOT expose partial writes, flushes, open
handles, or provider buffering state.

#### Scenario: Preserve a file after failed replacement

- **WHEN** a provider fails a replacement write after receiving its input
- **THEN** a later read observes the complete previous file and the write returns FileError

#### Scenario: Create a new file exactly once

- **WHEN** create-new is called twice for one path
- **THEN** the first call creates the complete file and the second fails with AlreadyExists without changing it

### Requirement: Directory observations are deterministic

`listDirectory` SHALL return owned entries sorted by normalized UTF-8 name bytes, independent of
provider enumeration order. Each entry SHALL expose only its portable name and kind: file or
directory. The portable service SHALL NOT expose symlink creation, inode identity, ownership,
permissions bits, timestamps, extended attributes, device entries, or host-specific metadata.

#### Scenario: Normalize provider enumeration

- **WHEN** a provider enumerates children in different physical orders
- **THEN** listDirectory returns the same lexically ordered portable entries

#### Scenario: Encounter unsupported host entry metadata

- **WHEN** a portable operation reaches an entry that cannot be represented as a portable file or directory
- **THEN** it fails with Unsupported rather than inventing portable semantics

### Requirement: File errors expose portable recovery reasons

`FileError` SHALL retain the operation, an owned portable Path, one closed semantic reason, and
optional provider detail. Reasons SHALL include NotFound, AlreadyExists, PermissionDenied,
InvalidPath, WrongType, NotEmpty, NoSpace, TooLarge, Unsupported, and Other. Provider-native codes
MUST remain diagnostic detail rather than control-flow tags. Allocation exhaustion SHALL remain a
separate failure channel.

#### Scenario: Translate a native missing-file code

- **WHEN** a native provider reports its platform-specific missing-file condition
- **THEN** FileSystem fails with NotFound and may retain the native code only as provider detail

#### Scenario: Fail in a virtual provider

- **WHEN** an in-memory provider rejects an operation with PermissionDenied
- **THEN** it returns the same portable reason without inventing a native code

### Requirement: PlatformFileSystem is an explicit portability escape hatch

`PlatformFileSystem` SHALL be a distinct lower-level service for host-native paths, handles,
seeking, mapping, locking, host metadata, links, and other behavior without a portable contract.
Portable standard-library operations MUST NOT require it directly. A native FileSystem provider MAY
use it internally. Depending on PlatformFileSystem SHALL be visible in a function's Effect
requirements and MUST NOT be satisfied by a portable FileSystem provider.

#### Scenario: Require native mapping explicitly

- **WHEN** a function opens a mapped native file through PlatformFileSystem
- **THEN** its contract exposes the platform requirement and the function cannot run against only a browser virtual FileSystem

### Requirement: Native and browser-capable providers preserve one service contract

The native provider SHALL map one explicitly configured provider root onto host storage and
translate lower-level failures into portable values. The in-memory provider SHALL implement the
same behavior deterministically without host access. Direct WebAssembly SHALL support a versioned
host-provider boundary capable of implementing the service over a browser virtual file system; it
MUST NOT require Unix file descriptors, process streams, or host-native paths.

#### Scenario: Read the same fixture on three providers

- **WHEN** native, in-memory, and hosted-Wasm providers expose the same portable tree
- **THEN** one Silk program observes identical bytes, entry kinds, directory order, and portable failures

### Requirement: FileSystem behavior agrees across execution engines

Equivalent provided programs SHALL preserve operation order, success values, FileError reasons,
owned-byte cleanup, and provider mutations through logical evaluation, native LLVM execution, and
direct WebAssembly. Backend artifacts and provider observations SHALL remain deterministic for
equivalent inputs.

#### Scenario: Replace and reread a file

- **WHEN** one Effect writes complete bytes and then reads the same path
- **THEN** evaluation, native execution, and direct WebAssembly observe the complete replacement exactly once
