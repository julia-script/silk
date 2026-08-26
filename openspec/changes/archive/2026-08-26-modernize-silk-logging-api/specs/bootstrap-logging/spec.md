## MODIFIED Requirements

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
