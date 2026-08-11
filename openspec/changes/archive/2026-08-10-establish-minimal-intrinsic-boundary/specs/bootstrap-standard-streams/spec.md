## ADDED Requirements

### Requirement: StandardStreams is a source-defined service

The `StandardStreams` contract, destination values, complete-write operation, typed failure, and
provider mappings SHALL be canonical Silk source. A native or hosted implementation MAY call one
complete-write intrinsic or private host import, but no compiler phase MAY recognize the
`StandardStreams`, `stdout`, `stderr`, or `writeAll` spellings to select special behavior.

#### Scenario: Write through the native implementation

- **WHEN** a provided native StandardStreams implementation receives one complete byte view
- **THEN** its source operation invokes one primitive complete-write boundary and preserves the service's typed result

#### Scenario: Replace the service without host output

- **WHEN** a pure in-memory implementation is provided
- **THEN** the same service call records the bytes without a platform intrinsic or host import
