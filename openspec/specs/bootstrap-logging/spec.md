# bootstrap-logging Specification

## Purpose

Define portable semantic logging as complete message invocations dispatched through an explicit
replaceable Logger service across native, test, telemetry-ready, and browser-capable hosts.

## Requirements

### Requirement: Log invocations receive complete semantic messages

A Logger invocation SHALL receive one `LogLevel` scalar enum value and one complete immutable
UTF-8 message view as separate operation parameters. `LogLevel` SHALL be a closed nominal enum with
the members `Trace`, `Debug`, `Info`, `Warning`, and `Error`; safe source SHALL NOT construct any
other severity value. `Effect.log` and every level-specific Effect logging operation MUST require
the complete message at invocation and MUST NOT expose a begin-event, byte append, stream handle,
flush, end-event, implicit destination, or partially committed event. The provider SHALL finish
consuming the borrowed message before the logging Effect completes; a provider that retains
observations MUST copy them into provider-owned storage.

#### Scenario: Select a severity nominally

- **WHEN** a program submits a logging invocation with `LogLevel.Warning`
- **THEN** the Logger receives that exact enum member without an open numeric severity wrapper

#### Scenario: Reject an undeclared severity

- **WHEN** safe source attempts to invent a logging severity outside the five declared `LogLevel` members
- **THEN** analysis rejects the value rather than admitting another enum inhabitant

#### Scenario: Submit one multiline message

- **WHEN** a program logs one message containing embedded line endings
- **THEN** the Logger receives one invocation with the complete message even if the provider renders several visual lines or physical writes

#### Scenario: Keep separate calls separate

- **WHEN** a program invokes `Effect.log` twice
- **THEN** the Logger receives two independently formatable invocations rather than one appendable stream

#### Scenario: Retain an event in memory

- **WHEN** an in-memory provider records a borrowed log message
- **THEN** later inspection observes provider-owned severity and message data after the caller's borrow has ended

### Requirement: Logging is an explicit replaceable service

`Logger` SHALL be a nominal service with one operation that accepts a severity and a complete
borrowed message and returns an Effect that can fail with `LogError`. A logging computation SHALL
retain its Logger requirement until an implementation is supplied through the ordinary Effect
provision model. Missing Logger provision MUST NOT silently discard an event or select a global
process logger.

#### Scenario: Reject a missing Logger

- **WHEN** a closed entry contains `Effect.log` and no Logger provider is supplied
- **THEN** analysis or execution reports the unsatisfied Logger requirement

#### Scenario: Replace the provider

- **WHEN** the same logging program is provided first with an in-memory Logger and then with a stdout-backed Logger
- **THEN** its source and Effect contract remain unchanged while each provider receives the event

### Requirement: Effect logging preserves order and typed failure

`Effect.log(message)` and `Effect.logInfo(message)` SHALL each submit an `Info` event.
`Effect.logTrace`, `Effect.logDebug`, `Effect.logWarning`, and `Effect.logError` SHALL submit the
corresponding `LogLevel` member, while `Effect.logAt(level, message)` SHALL submit its requested
member. Every helper SHALL retain the mutable Logger requirement and SHALL propagate `LogError`
unchanged. Events SHALL reach one provider in Effect execution order. A provider failure SHALL fail
the logging Effect, stop later dependent operations, and remain distinct from traps and allocation
failure.

#### Scenario: Use every level-specific helper

- **WHEN** a program invokes `logTrace`, `logDebug`, `log`, `logInfo`, `logWarning`, and `logError` in that order
- **THEN** the Logger observes `Trace`, `Debug`, `Info`, `Info`, `Warning`, and `Error` in the same order

#### Scenario: Select a level dynamically

- **WHEN** a program passes one `LogLevel` member to `Effect.logAt`
- **THEN** the Logger receives that member with the same message and Effect channels as a level-specific helper

#### Scenario: Preserve composed event order

- **WHEN** one Effect logs `first`, runs an ordinary computation, and logs `second`
- **THEN** the provider observes the two complete events in that order exactly once

#### Scenario: Propagate a provider failure

- **WHEN** the selected Logger rejects the first event with `LogError`
- **THEN** the logging Effect fails with that typed error and a later sequenced event is not submitted

### Requirement: Providers own rendering and physical output strategy

The Logger contract SHALL NOT prescribe textual prefixes, terminating newlines, destinations,
allocation, buffering, or the number and shape of underlying host or stream writes. The initial
stdout provider SHALL be able to forward the supplied borrowed message directly through
`StandardStreams` without mandatory allocation and SHALL translate a stream write failure into
`LogError`. Another provider MAY decorate, split, batch, retain, or structurally encode the same
invocation without changing the caller's Logger contract. No Logger requirement SHALL be satisfied
merely because StandardStreams is available.

#### Scenario: Forward one message directly

- **WHEN** the stdout Logger receives an info event whose message is `ready`
- **THEN** it can forward the borrowed message bytes without first constructing a decorated record

#### Scenario: Decorate in another provider

- **WHEN** another Logger adds a severity prefix, newline, color, or structured metadata
- **THEN** those choices remain local to that provider and do not change `Effect.log` or the Logger service contract

#### Scenario: Keep raw stdout independent

- **WHEN** a program writes bytes directly through StandardStreams
- **THEN** no semantic log invocation, severity, Logger requirement, or telemetry meaning is invented

### Requirement: In-memory logging is deterministic and host independent

The bootstrap in-memory Logger SHALL retain complete events in submission order for inspection and
SHALL perform no process, filesystem, console, clock, or telemetry access. Repeated equivalent runs
MUST expose equivalent event values and order.

#### Scenario: Capture without host output

- **WHEN** a test provides the in-memory Logger and runs a logging Effect
- **THEN** it reads the submitted events without any StandardStreams or host import observation

#### Scenario: Exhaust bounded bootstrap storage

- **WHEN** the bootstrap in-memory provider cannot retain another complete message
- **THEN** the logging Effect fails deterministically with `LogError` without adding an Allocator requirement to Logger

### Requirement: Logging agrees across execution engines

Equivalent provided logging programs SHALL preserve success, `LogError`, invocation order,
severity, and message content through pinned native LLVM execution and LLVM-generated WebAssembly.
WebAssembly MUST NOT require a Unix stream model; a host or Silk provider SHALL
satisfy Logger through the same service contract.

#### Scenario: Run logging under WebAssembly

- **WHEN** LLVM-generated WebAssembly executes a program with a compatible Logger provider
- **THEN** its complete invocation observations match the pinned native expectation without exposing byte-at-a-time logging to the caller
