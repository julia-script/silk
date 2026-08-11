## MODIFIED Requirements

### Requirement: Logging remains separate

This capability SHALL NOT define `Effect.log` as stdout writing. A separate Logger SHALL receive
complete semantic events rather than stream fragments and MAY route them to standard streams,
browser facilities, OpenTelemetry, tests, memory, or fan-out. A stdout-backed Logger is one
provider, not the meaning of logging; future default providers SHALL apply uniformly to services.

#### Scenario: Write without logging semantics

- **WHEN** an algorithm renders through `StandardStreams`
- **THEN** no log level, Logger requirement, or telemetry metadata is invented

#### Scenario: Render through the stdout Logger

- **WHEN** the stdout-backed Logger renders one complete LogEvent
- **THEN** StandardStreams receives exactly one complete write while the original call remains semantic logging
