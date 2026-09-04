# bootstrap-standard-streams Specification

## Purpose

Define the smallest explicit process-output service needed to observe real Silk programs while preserving separate Logger, default-provider, and Stream/Sink designs.

## Requirements

### Requirement: Standard streams are an explicit service requirement

`StandardStreams` SHALL provide stdout and stderr destinations as an explicitly required service. Programs using it SHALL retain that requirement until a provider is supplied; this change MUST NOT introduce ambient or Logger-specific defaults.

#### Scenario: Reject a missing provider

- **WHEN** a closed program requires `StandardStreams` without a provider
- **THEN** compilation or execution reports the unsatisfied requirement rather than discarding output

#### Scenario: Replace the provider

- **WHEN** a test supplies an in-memory provider
- **THEN** writes reach it without accessing host process streams

### Requirement: Byte writes are complete or fail explicitly

`StandardStreams.writeAll` SHALL accept one destination and immutable bytes, preserve call order, and either commit the complete sequence or return a typed stream failure. Formatting, structured logging, levels, span context, and buffering policy SHALL remain above this boundary.

#### Scenario: Write a complete byte sequence

- **WHEN** a program writes one byte view
- **THEN** the provider receives all bytes in order or the operation returns its typed failure

### Requirement: Native process destinations are explicit

Native execution SHALL connect an explicit provider to process destinations and preserve bytes,
ordering, destinations, and typed failures.

### Requirement: Logging remains separate

This capability SHALL NOT define `Effect.log` as stdout writing. A separate Logger SHALL receive
one complete semantic message per invocation rather than expose byte-fragment streaming to the
caller. A provider MAY route, decorate, split, batch, or retain that invocation through standard
streams, browser facilities, OpenTelemetry, tests, memory, or fan-out. The Logger contract SHALL
NOT prescribe the number or shape of underlying StandardStreams writes. A stdout-backed Logger is
one provider, not the meaning of logging; future default providers SHALL apply uniformly to services.

#### Scenario: Write without logging semantics

- **WHEN** an algorithm renders through `StandardStreams`
- **THEN** no log level, Logger requirement, or telemetry metadata is invented

#### Scenario: Render through the stdout Logger

- **WHEN** the stdout-backed Logger renders one complete invocation
- **THEN** its chosen physical writes remain provider-local while the original call remains one semantic logging invocation

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
