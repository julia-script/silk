## ADDED Requirements

### Requirement: OsFileSystem is separate ordinary source

Canonical standard-library source SHALL define `OsFileSystem` as an ordinary provider separate from
the portable `FileSystem`, `Path`, and value actors. Its constructor SHALL copy one absolute native
root into owned `Bytes` and SHALL require `OutOfMemory ? &mut Allocator`. Portable service signatures
MUST NOT mention `OsHandle`, native paths, target selectors, or the provider type.

#### Scenario: Construct an owned native root

- **WHEN** an application creates `OsFileSystem` from borrowed native-root bytes
- **THEN** the provider owns an independent copy and the caller may release the original bytes

#### Scenario: Navigate provider implementation

- **WHEN** tooling resolves an `OsFileSystem` constructor or service method
- **THEN** it navigates to canonical Silk source while only the enclosed low-level calls resolve to `Intrinsic`

#### Scenario: Replace the provider lexically

- **WHEN** an application supplies another value conforming to the portable `FileSystem` service
- **THEN** Effect dispatch uses that provider without constructing `OsFileSystem` or requiring OS intrinsics
