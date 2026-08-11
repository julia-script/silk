## ADDED Requirements

### Requirement: Native backends link only reachable OS runtime operations

Supported native LLVM emission SHALL lower each validated reachable OS intrinsic to its compiler-owned
runtime operation and link only the required support symbols. Runtime operations SHALL preserve
opaque affine handle identity, transferred-byte counts, normalized reasons, native codes, consuming
close, root confinement, and retryable directory iteration.

#### Scenario: Emit a native whole-file reader

- **WHEN** a native executable reaches file open, repeated read, and close through `OsFileSystem`
- **THEN** LLVM emits and links those runtime operations with the same observable protocol as evaluation

#### Scenario: Omit unused OS runtime support

- **WHEN** a native program reaches no OS filesystem intrinsic
- **THEN** its artifact and link plan contain no OS filesystem runtime symbols

### Requirement: Direct Wasm receives no implicit OS filesystem ABI

Direct-Wasm emission MUST NOT lower OS filesystem intrinsics to invented imports, JavaScript shims,
WASI calls, or a built-in virtual filesystem. Generic target-availability validation SHALL reject a
reachable OS intrinsic before backend construction and SHALL allow programs whose executable closure
does not contain one.

#### Scenario: Reject reachable OS operations

- **WHEN** a direct-Wasm entry reaches `Intrinsic.osFileOpen`
- **THEN** planning reports target unavailability and no partial Wasm module is constructed

#### Scenario: Emit a user-provided portable implementation

- **WHEN** a direct-Wasm program supplies an ordinary source-defined `FileSystem` and reaches no OS intrinsic
- **THEN** emission succeeds without filesystem imports

