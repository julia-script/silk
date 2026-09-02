## ADDED Requirements

### Requirement: HostInput and its native provider are separate canonical modules

Canonical standard-library source SHALL define the portable `HostInput` service, its typed failure,
and its byte and checked-text helpers in one module, and the native `OsHostInput` provider in
another, mirroring the portable `FileSystem` and native `OsFileSystem` split. The portable signature
MUST NOT mention the provider type, native storage, or target selectors.

#### Scenario: Implement the service without a platform intrinsic

- **WHEN** an application supplies its own value conforming to `HostInput`
- **THEN** Effect dispatch uses that provider without constructing `OsHostInput` or requiring an OS intrinsic

#### Scenario: Navigate provider implementation

- **WHEN** tooling resolves an `OsHostInput` lookup
- **THEN** it navigates to canonical Silk source while only the enclosed low-level call resolves to `Intrinsic`
