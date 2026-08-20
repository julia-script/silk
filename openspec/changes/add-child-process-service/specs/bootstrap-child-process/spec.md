## Purpose

Define the smallest explicit child-process service needed for a self-hosted Silk compiler to invoke
another program, and its native provider's behavior.

## ADDED Requirements

### Requirement: Child-process execution is an explicit service

`ChildProcess` SHALL be a service with exactly one blocking `execute` operation, required explicitly
until a provider is supplied. The operation SHALL run one child to completion before it returns.
This capability MUST NOT introduce an ambient default provider, asynchronous or concurrent
execution, pipes, streaming output, interactive child input, process groups, signal delivery, or job
control.

#### Scenario: Reject a missing provider

- **WHEN** a closed program requires `ChildProcess` without a provider
- **THEN** compilation or execution reports the unsatisfied requirement rather than inventing an outcome

#### Scenario: Replace the provider

- **WHEN** a test supplies a pure in-source provider
- **THEN** executions reach it without a host boundary or platform intrinsic

### Requirement: The service never interprets a shell command string

The request SHALL name an executable directly. No part of a request SHALL be parsed as a command
line, expanded, split on whitespace, or given to a shell, and the service MUST NOT search a
executable path variable.

#### Scenario: Pass a metacharacter as data

- **WHEN** an argument contains a space, a quote, or a shell metacharacter
- **THEN** the child receives that argument as one exact value rather than as syntax

### Requirement: A request carries a program, ordered arguments, an environment, and a directory

`ProcessRequest` SHALL carry an executable path, ordered arguments, an exact environment, and an
optional working directory. Arguments and environment values SHALL be exact platform bytes rather
than checked text, and SHALL reach the child unchanged. Arguments SHALL retain the order they were
added. The environment SHALL start empty, so the service does not read the caller's own environment
by default. Omitting the working directory SHALL mean the child inherits the caller's.

#### Scenario: Retain argument order

- **WHEN** a request adds several arguments
- **THEN** the child receives them in that order after its program name

#### Scenario: Carry a value that is not well-formed text

- **WHEN** an argument or environment value holds bytes that are not well-formed text
- **THEN** the request carries those exact bytes and no validation rejects or replaces them

#### Scenario: Start from an empty environment

- **WHEN** a request names no environment entry
- **THEN** the execution presents no variable at all rather than the caller's own environment

### Requirement: The outcome distinguishes an exit code from a terminating signal

The execution outcome SHALL be `Exited` carrying an exit code or `Signaled` carrying a terminating
signal number. Both SHALL own the complete captured standard output and the complete captured
standard error. A signaled outcome MUST NOT present a signal number as an exit code.

#### Scenario: Own what the child wrote

- **WHEN** a child writes to standard output and to standard error
- **THEN** the outcome owns both complete captures independently of the provider

#### Scenario: Report a terminating signal

- **WHEN** a signal terminates the child
- **THEN** the outcome is `Signaled` with that signal number and no exit code is available

### Requirement: A nonzero exit code is result data

An execution whose child ran and returned a nonzero code SHALL return an `Exited` outcome carrying
that code. It MUST NOT enter the typed failure channel.

#### Scenario: Read a failing tool's result

- **WHEN** a child exits with a nonzero code
- **THEN** the caller reads that code and the captured output without handling a typed failure

### Requirement: A failure to start, wait, or capture is a typed process failure

A provider that cannot start the child, cannot wait for it, or cannot capture its output SHALL
return `ProcessError`. The failure SHALL name which of those stages failed, carry a closed
portable reason, and MAY retain a numeric provider detail.

#### Scenario: Surface a missing executable

- **WHEN** no executable exists at the requested path
- **THEN** the operation returns `ProcessError` naming the start stage rather than an exit code

### Requirement: The child's standard input is closed

The service SHALL close the child's standard input. A child that reads its standard input SHALL
observe an immediate end of input, and the caller SHALL have no way to write to it.

#### Scenario: Read from a closed input

- **WHEN** a child reads its standard input
- **THEN** it observes the end of input immediately and the execution still completes

### Requirement: The native provider executes through unsafe OS primitives

Canonical standard-library source SHALL define `OsChildProcess` as an ordinary provider that runs
one child through one unsafe `Intrinsic` operation and copies the completed capture through another,
using the same low-level reason and native-code outputs as the OS filesystem operations. No compiler
phase MAY recognize the `ChildProcess`, `OsChildProcess`, `ProcessRequest`, `ProcessOutcome`, or
`execute` spellings to select special behavior.

#### Scenario: Execute through the native implementation

- **WHEN** a provided native implementation receives one request
- **THEN** its source operation invokes the primitive execution boundary and preserves the service's outcome and typed failure

#### Scenario: Reject the native execution on direct WebAssembly

- **WHEN** a reachable native execution is compiled for a direct WebAssembly target
- **THEN** target availability rejects it rather than inventing a process host import

#### Scenario: Link only the reachable runtime symbols

- **WHEN** a native program executes a child and touches no filesystem
- **THEN** the artifact links the child-process runtime symbols and no filesystem runtime symbol
