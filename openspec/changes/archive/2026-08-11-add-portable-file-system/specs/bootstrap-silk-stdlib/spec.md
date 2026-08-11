## ADDED Requirements

### Requirement: Portable filesystem actors are canonical ordinary Silk source

The standard library SHALL ship canonical `.silk` declarations for `Path`, `FileInfo`,
`DirectoryInfo`, `DirectoryEntry`, `FileOperation`, `FileReason`, `FileError`, and `FileSystem` plus
the ordinary helpers `createDirectoriesRecursively`, `writeFileWithParents`, and `exists`. These
actors SHALL participate in the deterministic standard-library manifest, retain ordinary source
spans in diagnostics and editor facts, and receive no compiler privilege from names or module origin.
`Bytes` SHALL be consumed from the separate owned-bytes foundation rather than reimplemented here.

#### Scenario: Navigate to a primitive operation

- **WHEN** editor tooling resolves `FileSystem.readFile`
- **THEN** go-to-definition opens canonical shipped Silk source rather than a compiler-generated host signature

#### Scenario: Navigate to a helper

- **WHEN** editor tooling resolves `writeFileWithParents`
- **THEN** go-to-definition opens its ordinary source composition over parent creation and `writeFile`

#### Scenario: Define a user service implementation

- **WHEN** a user value satisfies the source-defined `FileSystem` contract
- **THEN** it receives ordinary service provision, ownership, and lowering behavior without intrinsic registration

### Requirement: Portable source has no platform provider dependency

Portable filesystem actors and helpers MUST NOT import an OS provider, native path or handle type,
host target selector, hosted-Wasm ABI, or built-in virtual provider. Applications SHALL select and
provide implementations at their outer boundary.

#### Scenario: Load portable source on direct Wasm

- **WHEN** a direct-Wasm application imports portable Path and FileSystem declarations
- **THEN** module closure requires no operating-system filesystem implementation or host import

#### Scenario: Supply an application-specific provider

- **WHEN** an application defines a virtual provider in ordinary Silk source
- **THEN** it can satisfy FileSystem without depending on any standard-library platform implementation
