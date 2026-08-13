## ADDED Requirements

### Requirement: ChildProcess and its native provider are separate canonical modules

Canonical standard-library source SHALL define the portable `ChildProcess` service, its
`ProcessRequest` builder, its `ProcessOutcome` members, and its typed process failure in one module,
and the native `OsChildProcess` provider in another, mirroring the portable `FileSystem` and native
`OsFileSystem` split. The portable signature MUST NOT mention the provider type, native descriptors,
or target selectors.

#### Scenario: Implement the service without a platform intrinsic

- **WHEN** an application supplies its own value conforming to `ChildProcess`
- **THEN** Effect dispatch uses that provider without constructing `OsChildProcess` or requiring an OS intrinsic

#### Scenario: Navigate provider implementation

- **WHEN** tooling resolves an `OsChildProcess` execution
- **THEN** it navigates to canonical Silk source while only the enclosed low-level calls resolve to `Intrinsic`
