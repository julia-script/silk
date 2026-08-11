## ADDED Requirements

### Requirement: Portable filesystem actors are canonical visible Silk source

The standard library SHALL ship canonical `.silk` declarations for Bytes, Path, PathSlice,
DirectoryEntry, FileError, FileSystem, and the PlatformFileSystem boundary. The portable operations
and provider-facing contracts SHALL participate in the deterministic standard-library manifest,
retain ordinary source spans in diagnostics and editor facts, and receive no compiler privilege
from their names or module origin.

#### Scenario: Navigate to readFile

- **WHEN** editor tooling resolves a FileSystem read operation
- **THEN** go-to-definition opens canonical shipped Silk source rather than a generated host signature

#### Scenario: Define a user filesystem capability

- **WHEN** a user module defines an equivalent whole-file service under another legal name
- **THEN** it receives ordinary conformance, Effect requirement, ownership, and lowering behavior without intrinsic registration

### Requirement: Portable APIs prefer FileSystem over PlatformFileSystem

Canonical standard-library modules that can express their behavior through portable paths and
complete values SHALL require FileSystem rather than PlatformFileSystem. PlatformFileSystem SHALL be
imported only by provider implementations or APIs whose documented contract is deliberately
host-specific.

#### Scenario: Inspect a portable standard-library operation

- **WHEN** a user navigates to a standard operation that reads a complete file
- **THEN** its source contract requires FileSystem and contains no native handle or platform path type
