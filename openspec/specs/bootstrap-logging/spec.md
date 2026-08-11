# bootstrap-logging Specification

## Purpose

Define portable semantic logging as complete message invocations dispatched through an explicit
replaceable Logger service across native, test, telemetry-ready, and browser-capable hosts.

## Requirements

### Requirement: Log invocations receive complete semantic messages

A Logger invocation SHALL receive a closed severity and one complete immutable UTF-8 message view
as separate operation parameters. The bootstrap severities SHALL be trace, debug, info, warning,
and error. `Effect.log` MUST require the complete message at invocation and MUST NOT expose a
begin-event, byte append, stream handle, flush, end-event, implicit destination, or partially
committed event. The provider SHALL finish consuming the borrowed message before the logging Effect
completes; a provider that retains observations MUST copy them into provider-owned storage.

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
severity, and message content through logical evaluation, native LLVM execution, and direct
WebAssembly. Direct WebAssembly MUST NOT require a Unix stream model; a host or Silk provider SHALL
satisfy Logger through the same service contract.

#### Scenario: Run logging under WebAssembly

- **WHEN** direct WebAssembly executes a program with a compatible Logger provider
- **THEN** its complete invocation observations match logical evaluation without exposing byte-at-a-time logging to the caller
