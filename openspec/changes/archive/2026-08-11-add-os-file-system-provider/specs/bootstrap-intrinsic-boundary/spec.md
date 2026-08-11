## ADDED Requirements

### Requirement: OS filesystem privilege is handle-level and sealed

The `Intrinsic` namespace SHALL contain only the unsafe file open/read/write, directory open/next,
path inspection, directory creation, file removal, directory removal, and generic consuming close
operations required to build an OS provider. Their signatures SHALL use primitive scalars, slices,
output parameters, `Option`, `bool`, and opaque `OsHandle`; they MUST NOT use a source-defined
filesystem service or domain value.

#### Scenario: Build a source provider from low-level calls

- **WHEN** canonical `OsFileSystem` implements a whole-file read
- **THEN** it composes open, repeated read, and consuming close rather than invoking a compiler-known whole-file operation

#### Scenario: Keep portable operations ordinary

- **WHEN** another source-defined provider implements `FileSystem.readFile`
- **THEN** it can satisfy the service without invoking any OS intrinsic or receiving name-based compiler treatment

### Requirement: OS intrinsic invariants are explicit and unsafe

Each OS intrinsic contract SHALL document handle kind and liveness, initialized input/output ranges,
root-confinement inputs, retry behavior, consuming behavior, and target availability. Semantic
analysis SHALL reject safe-call syntax for these operations while preserving the sealed catalog as
the only source-callable compiler privilege.

#### Scenario: Require unsafe acknowledgement

- **WHEN** ordinary source calls an OS handle operation outside the language's unsafe call form
- **THEN** semantic analysis rejects the call even if its argument types otherwise match

#### Scenario: Consume close regardless of outcome

- **WHEN** consuming close reports failure
- **THEN** the source handle is nevertheless dead and cannot be retried or dropped as a live resource

