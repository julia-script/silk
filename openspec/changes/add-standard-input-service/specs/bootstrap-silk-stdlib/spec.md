## ADDED Requirements

### Requirement: StandardInput and its native provider are separate canonical modules

Canonical standard-library source SHALL define the portable `StandardInput` service, its
`ReadOutcome` members, and its typed read failure in one module, and the native `OsStandardInput`
provider in another, mirroring the portable `FileSystem` and native `OsFileSystem` split. The
portable signature MUST NOT mention the provider type, native descriptors, or target selectors.

#### Scenario: Implement the service without a platform intrinsic

- **WHEN** an application supplies its own value conforming to `StandardInput`
- **THEN** Effect dispatch uses that provider without constructing `OsStandardInput` or requiring an OS intrinsic

#### Scenario: Navigate provider implementation

- **WHEN** tooling resolves an `OsStandardInput` read
- **THEN** it navigates to canonical Silk source while only the enclosed low-level call resolves to `Intrinsic`
