## MODIFIED Requirements

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
