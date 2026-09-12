## MODIFIED Requirements

### Requirement: Log invocations receive complete semantic messages

A Logger invocation SHALL receive one runtime `LogLevel` scalar enum value, one static UTF-8
template, and one borrowed tuple or record argument pack. The template and argument pack SHALL be
interpreted with the `template-formatting` capability to produce one complete semantic message.
`LogLevel` SHALL be a closed nominal enum with the members `Trace`, `Debug`, `Info`, `Warning`, and
`Error`; safe source SHALL NOT construct any other severity value. `Effect.log` and every
level-specific Effect logging operation MUST accept the static template and borrowed pack and MUST
NOT expose a begin-event, byte append, stream handle, flush, end-event, implicit destination, or
partially committed retained event. The provider SHALL finish consuming the borrowed pack before
the logging Effect completes; a provider that retains observations MUST copy the completed message
into provider-owned storage.

#### Scenario: Select a severity nominally

- **WHEN** a program submits a logging invocation with `LogLevel.Warning`
- **THEN** the Logger receives that exact enum member without an open numeric severity wrapper

#### Scenario: Reject an undeclared severity

- **WHEN** safe source attempts to invent a logging severity outside the five declared `LogLevel` members
- **THEN** analysis rejects the value rather than admitting another enum inhabitant

#### Scenario: Submit one multiline message

- **WHEN** a template and its displayed values produce embedded line endings
- **THEN** the Logger observes one complete semantic invocation even if the provider performs several physical writes or renders several visual lines

#### Scenario: Keep separate calls separate

- **WHEN** a program invokes `Effect.log` twice with templates and argument packs
- **THEN** the Logger receives two independently formatable invocations rather than one appendable stream

#### Scenario: Retain an event in memory

- **WHEN** an in-memory provider formats and records a borrowed argument pack
- **THEN** later inspection observes provider-owned severity and complete message data after the caller's borrow has ended

### Requirement: Logging is an explicit replaceable service

`Logger` SHALL be a nominal service with one generic operation that accepts a runtime severity, a
static template, and one borrowed argument pack and returns an Effect that can fail with `LogError`.
A logging computation SHALL retain its Logger requirement until an implementation is supplied
through the ordinary Effect provision model. Writer, Allocator, `WriterError`, and
`OutOfMemoryError` SHALL NOT appear in the public logging Effect's requirements or failure row.
Missing Logger provision MUST NOT silently discard an event or select a global process logger.

#### Scenario: Reject a missing Logger

- **WHEN** a closed entry contains `Effect.log("ready", &())` and no Logger provider is supplied
- **THEN** analysis or execution reports the unsatisfied Logger requirement

#### Scenario: Replace the provider

- **WHEN** the same template logging program is provided first with an in-memory Logger and then with a stdout-backed Logger
- **THEN** its source and Effect contract remain unchanged while each provider formats and receives the event

#### Scenario: Keep formatting capabilities inside the provider

- **WHEN** a caller logs a tuple or record argument pack
- **THEN** the resulting Effect requires only mutable Logger and can fail only with LogError

### Requirement: Effect logging preserves order and typed failure

`Effect.log(template, args)` and `Effect.logInfo(template, args)` SHALL each submit an `Info` event.
`Effect.logTrace`, `Effect.logDebug`, `Effect.logWarning`, and `Effect.logError` SHALL submit the
corresponding `LogLevel` member, while `Effect.logAt(level, template, args)` SHALL accept a runtime
level and submit that requested member. Every helper SHALL retain the mutable Logger requirement and
SHALL propagate `LogError` unchanged. Events SHALL reach one provider in Effect execution order. A
provider failure SHALL fail the logging Effect, stop later dependent operations, and remain distinct
from traps and allocation failure.

#### Scenario: Use every level-specific helper

- **WHEN** a program invokes `logTrace`, `logDebug`, `log`, `logInfo`, `logWarning`, and `logError` in that order
- **THEN** the Logger observes `Trace`, `Debug`, `Info`, `Info`, `Warning`, and `Error` in the same order

#### Scenario: Select a level dynamically

- **WHEN** a program passes one runtime `LogLevel` value to `Effect.logAt`
- **THEN** the Logger receives that member with the same formatted message and Effect channels as a level-specific helper

#### Scenario: Preserve composed event order

- **WHEN** one Effect logs `first`, runs an ordinary computation, and logs `second`
- **THEN** the provider observes the two complete events in that order exactly once

#### Scenario: Propagate a provider failure

- **WHEN** the selected Logger rejects the first event with `LogError`
- **THEN** the logging Effect fails with that typed error and a later sequenced event is not submitted

### Requirement: Providers own rendering and physical output strategy

The Logger contract SHALL NOT prescribe textual prefixes, terminating newlines, destinations,
allocation, buffering, or the number and shape of underlying host or stream writes. Each provider
SHALL apply the shared template-formatting grammar, validation, field projection, and Display
selection rather than a logging-specific formatter. The initial stdout provider SHALL format
directly through its Writer without mandatory message allocation and SHALL translate a stream write
failure into `LogError`. Another provider MAY decorate, split, batch, retain, or structurally encode
the same invocation without changing the caller's Logger contract. No Logger requirement SHALL be
satisfied merely because a Writer is available.

#### Scenario: Forward one message directly

- **WHEN** the stdout Logger receives `Effect.log("ready {}", &(42,))`
- **THEN** it may perform several physical Writer operations without allocating an intermediate message or creating more than one semantic Logger invocation

#### Scenario: Decorate in another provider

- **WHEN** another Logger adds a severity prefix, newline, color, or structured metadata
- **THEN** those choices remain local to that provider and do not change the Effect logging contract

#### Scenario: Keep raw stdout independent

- **WHEN** a program writes bytes directly through Writer
- **THEN** no semantic log invocation, severity, Logger requirement, or telemetry meaning is invented

#### Scenario: Preserve a physical prefix on failure

- **WHEN** a streaming provider fails after accepting an earlier formatted segment
- **THEN** the logging Effect fails with LogError and the provider MAY already have committed that physical prefix without committing a retained semantic event

### Requirement: In-memory logging is deterministic and host independent

The bootstrap in-memory Logger SHALL format and retain complete events in submission order for
inspection and SHALL perform no process, filesystem, console, clock, allocator, or telemetry access.
Repeated equivalent runs MUST expose equivalent event values and order. A failed attempt SHALL
increase attempt accounting but SHALL NOT commit a level, length, message prefix, or event.

#### Scenario: Capture without host output

- **WHEN** a test provides the in-memory Logger and logs a statically validated template
- **THEN** it reads the complete formatted event without any Writer or host import requirement at the call site

#### Scenario: Exhaust bounded bootstrap storage

- **WHEN** the bootstrap in-memory provider cannot retain the complete formatted message
- **THEN** the logging Effect fails deterministically with `LogError`, records one attempt, and commits no part of the event

#### Scenario: Reject a configured attempt

- **WHEN** the in-memory provider is configured to reject a logging attempt
- **THEN** no template segment or displayed value becomes observable in retained storage

### Requirement: Logging agrees across execution engines

Equivalent provided template logging programs SHALL preserve success, `LogError`, invocation order,
severity, template validation, and formatted message content through pinned native LLVM execution and
LLVM-generated WebAssembly. WebAssembly MUST NOT require a Unix stream model; a host or Silk provider
SHALL satisfy Logger through the same service contract.

#### Scenario: Run logging under WebAssembly

- **WHEN** LLVM-generated WebAssembly executes a program with a compatible Logger provider and borrowed argument pack
- **THEN** its complete formatted invocation observations match the pinned native expectation without exposing Writer operations to the caller
