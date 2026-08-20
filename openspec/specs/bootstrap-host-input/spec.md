# bootstrap-host-input Specification

## Purpose

Define the smallest explicit process-input service needed for a real Silk program to learn what it
was started with — its command line, its environment, and its working directory — the representation
those values carry, and its native provider's behavior.

## Requirements

### Requirement: Host input is an explicit read-only service

`HostInput` SHALL be a service required explicitly until a provider is supplied, exposing the
argument count, one argument by index, one environment value by name, and the working directory.
The service SHALL read only: it MUST NOT expose an operation that sets an environment variable or
changes the working directory, and it MUST NOT introduce an ambient default provider.

#### Scenario: Reject a missing provider

- **WHEN** a closed program requires `HostInput` without a provider
- **THEN** compilation or execution reports the unsatisfied requirement rather than inventing an empty command line

#### Scenario: Replace the provider

- **WHEN** a test supplies a pure in-source provider with a scripted command line
- **THEN** the lookups reach it without a host boundary or platform intrinsic

### Requirement: Arguments are ordered and complete

The service SHALL report the number of arguments the process received, including the program name at
index zero, and SHALL return the argument at any index below that count. Collecting the arguments
SHALL preserve the order the process received them.

#### Scenario: Return three arguments in order

- **WHEN** a program collects the arguments of a process started with three of them
- **THEN** it observes exactly those three values in the order the process received them

#### Scenario: Report the count including the program name

- **WHEN** a program asks for the argument count
- **THEN** the count includes the program name at index zero

### Requirement: Host values are raw bytes with a checked textual view

Every value the service returns SHALL be the raw bytes the process received, unchanged. A textual
view SHALL be available, SHALL be checked, and SHALL be fallible; it MUST NOT replace, substitute, or
discard bytes it cannot decode, and the bytes SHALL remain readable after it fails. This capability
MUST NOT introduce a second text type.

#### Scenario: Preserve a value that is not valid UTF-8

- **WHEN** an environment value or argument is not valid UTF-8
- **THEN** the program reads its exact bytes and can pass them on unchanged

#### Scenario: Refuse to decode undecodable bytes

- **WHEN** the checked textual view is applied to bytes that are not valid UTF-8
- **THEN** it reports the decoding failure and the original bytes stay available

### Requirement: Absence is data and a broken host is a failure

An index at or past the argument count, and an environment name that is not set, SHALL be reported as
absence rather than a typed failure. `HostInputError` SHALL mean only that the host could not
answer the lookup at all.

#### Scenario: Report an unset variable

- **WHEN** a program reads an environment variable that is not set
- **THEN** the lookup reports absence and no typed failure is raised

#### Scenario: Report an index past the last argument

- **WHEN** a program asks for an argument at or past the argument count
- **THEN** the lookup reports absence rather than failing

#### Scenario: Surface a refused lookup

- **WHEN** the host cannot answer a lookup
- **THEN** the operation returns `HostInputError` and the caller's recovery branch observes it

### Requirement: The working directory is a host-absolute byte value

The service SHALL return the process working directory as raw bytes. It MUST NOT be returned as a
provider-namespace `Path`, because that type is confined to a selected provider's root and requires
valid UTF-8, and it SHALL NOT have an absent case: a host that cannot answer is a failure.

#### Scenario: Read the working directory

- **WHEN** a program asks for its working directory
- **THEN** it receives the host-absolute location as raw bytes

### Requirement: The native provider reads through unsafe OS primitives

Canonical standard-library source SHALL define `OsHostInput` as an ordinary provider reading the
process command line, environment block, and working directory through unsafe `Intrinsic` operations
using the same `Option<usize>` result with low-level reason and native-code outputs as the OS
filesystem operations. A success SHALL report the value's complete byte length and copy the prefix
that fits, so an undersized buffer is completed by one exactly sized second pass. The not-found
reason SHALL become absence and any other reason SHALL become `HostInputError`. No compiler phase
MAY recognize the `HostInput`, `OsHostInput`, or operation spellings to select special behavior.

#### Scenario: Complete a value longer than the provider buffer

- **WHEN** a value is longer than the buffer the provider first offered
- **THEN** the provider learns its complete length and returns the complete value

#### Scenario: Reject the native lookups on direct WebAssembly

- **WHEN** a reachable native host-input lookup is compiled for a direct WebAssembly target
- **THEN** target availability rejects it rather than inventing a process-input import

#### Scenario: Link only the reachable runtime symbols

- **WHEN** a native program reads host input and touches no filesystem
- **THEN** the artifact links the host-input runtime symbols and no filesystem runtime symbol

### Requirement: Argument interpretation stays above this boundary

This capability SHALL NOT define argument parsing, flag or option grammar, subcommand dispatch,
configuration layering, environment mutation, or working-directory mutation.

#### Scenario: Read arguments without a grammar

- **WHEN** a program reads its arguments through the service
- **THEN** no flag syntax, option value, or subcommand is interpreted for it
