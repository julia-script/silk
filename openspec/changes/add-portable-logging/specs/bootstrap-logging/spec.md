## Purpose

Define portable semantic logging as complete structured events dispatched through an explicit
replaceable Logger service across native, test, telemetry-ready, and browser-capable hosts.

## ADDED Requirements

### Requirement: Log events are complete semantic values

A `LogEvent` SHALL contain a closed severity and one complete immutable UTF-8 message view. The
bootstrap severities SHALL be trace, debug, info, warning, and error. Submitting an event MUST NOT
expose a byte-at-a-time append, stream handle, implicit destination, or partially committed event.
The provider SHALL finish consuming the borrowed event before the logging Effect completes; a
provider that retains events MUST copy them into provider-owned storage.

#### Scenario: Submit one multiline message

- **WHEN** a program logs one message containing embedded line endings
- **THEN** the Logger receives one event with the complete message rather than one event per line or stream fragment

#### Scenario: Retain an event in memory

- **WHEN** an in-memory provider records a borrowed log event
- **THEN** later inspection observes provider-owned severity and message data after the caller's borrow has ended

### Requirement: Logging is an explicit replaceable service

`Logger` SHALL be a nominal service capability with one operation that accepts a complete event and
returns an Effect that can fail with `LogError`. A logging computation SHALL retain its Logger
requirement until an implementation is supplied through the ordinary Effect provision model.
Missing Logger provision MUST NOT silently discard an event or select a global process logger.

#### Scenario: Reject a missing Logger

- **WHEN** a closed entry contains `Effect.log` and no Logger provider is supplied
- **THEN** analysis or execution reports the unsatisfied Logger requirement

#### Scenario: Replace the provider

- **WHEN** the same logging program is provided first with an in-memory Logger and then with a stdout-backed Logger
- **THEN** its source and Effect contract remain unchanged while each provider receives the event

### Requirement: Effect logging preserves order and typed failure

`Effect.log(message)` SHALL submit an info event, and the level-selecting logging operation SHALL
submit the requested severity. Events SHALL reach one provider in Effect execution order. A
provider failure SHALL fail the logging Effect with `LogError`, stop later dependent operations, and
remain distinct from traps and allocation failure.

#### Scenario: Preserve composed event order

- **WHEN** one Effect logs `first`, runs an ordinary computation, and logs `second`
- **THEN** the provider observes the two complete events in that order exactly once

#### Scenario: Propagate a provider failure

- **WHEN** the selected Logger rejects the first event with `LogError`
- **THEN** the logging Effect fails with that typed error and a later sequenced event is not submitted

### Requirement: The stdout Logger is a provider, not logging semantics

The initial live Logger SHALL render each event deterministically as UTF-8 containing its severity,
message, and one terminating newline, then submit that complete rendering through exactly one
`StandardStreams.writeAll` call to stdout. It SHALL translate a stream write failure into
`LogError` without changing the original semantic event contract. No Logger requirement SHALL be
satisfied merely because StandardStreams is available.

#### Scenario: Render one complete stdout event

- **WHEN** the stdout Logger receives an info event whose message is `ready`
- **THEN** StandardStreams receives one complete deterministic write containing the severity, message, and terminating newline

#### Scenario: Keep raw stdout independent

- **WHEN** a program writes bytes directly through StandardStreams
- **THEN** no LogEvent, severity, Logger requirement, or telemetry meaning is invented

### Requirement: In-memory logging is deterministic and host independent

The bootstrap in-memory Logger SHALL retain complete events in submission order for inspection and
SHALL perform no process, filesystem, console, clock, or telemetry access. Repeated equivalent runs
MUST expose equivalent event values and order.

#### Scenario: Capture without host output

- **WHEN** a test provides the in-memory Logger and runs a logging Effect
- **THEN** it reads the submitted events without any StandardStreams or host import observation

### Requirement: Logging agrees across execution engines

Equivalent provided logging programs SHALL preserve success, `LogError`, event order, severity, and
message content through logical evaluation, native LLVM execution, and direct WebAssembly. Direct
WebAssembly MUST NOT require a Unix stream model; a host or Silk provider SHALL satisfy Logger
through the same service contract.

#### Scenario: Run logging under WebAssembly

- **WHEN** direct WebAssembly executes a program with a compatible Logger provider
- **THEN** its complete event observations match logical evaluation without exposing byte-at-a-time output
