# bootstrap-standard-input Specification

## Purpose

Define the smallest explicit process-input service needed for real Silk programs to read bytes,
separate from the write-only standard-streams contract, and its native provider's behavior.

## Requirements

### Requirement: Standard input is a separate explicit service

`StandardInput` SHALL be a service distinct from `StandardStreams`, required explicitly until a
provider is supplied. This capability MUST NOT change `StandardStreams`, its `bool` destination, its
`writeAll` operation, or its typed write failure, and MUST NOT introduce an ambient default input
provider.

#### Scenario: Reject a missing provider

- **WHEN** a closed program requires `StandardInput` without a provider
- **THEN** compilation or execution reports the unsatisfied requirement rather than inventing empty input

#### Scenario: Replace the provider

- **WHEN** a test supplies a pure in-source provider
- **THEN** reads reach it without a host boundary or platform intrinsic

### Requirement: A read reports the exact count it committed

`StandardInput.read` SHALL accept one exclusive byte buffer, fill a prefix of it, and report the
exact number of bytes it committed. The reported count MAY be less than the buffer length and SHALL
NOT exceed it. Bytes past the committed prefix SHALL remain unchanged.

#### Scenario: Fill a complete buffer

- **WHEN** the provider commits as many bytes as the buffer holds
- **THEN** the outcome reports that count and the buffer holds exactly those bytes in order

#### Scenario: Report a partial read

- **WHEN** the provider commits fewer bytes than the buffer holds
- **THEN** the outcome reports the true committed count rather than the buffer length, and the remaining buffer bytes are untouched

### Requirement: End of input is outcome data, not a failure

The read outcome SHALL be `Filled` carrying a committed count or `EndOfInput`. Reaching the end of
input SHALL be reported as `EndOfInput` and MUST NOT enter the typed failure channel, so a caller
that drains input to completion handles no failure on that path.

#### Scenario: Drain input to its end

- **WHEN** a program reads until the provider reports that no further bytes will arrive
- **THEN** the final outcome is `EndOfInput`, its count is zero, and no typed failure is raised

#### Scenario: Distinguish end of input from a short read

- **WHEN** one read commits fewer bytes than requested and a later read reaches the end
- **THEN** the first outcome is `Filled` with its true count and only the later outcome is `EndOfInput`

### Requirement: A host error is a typed read failure

A provider that cannot perform a read SHALL return `StreamReadError`. The failure SHALL be
distinct from the write-side stream failure and SHALL NOT be used to signal the end of input.

#### Scenario: Surface a refused read

- **WHEN** the host refuses a read
- **THEN** the operation returns `StreamReadError` and the caller's recovery branch observes it

### Requirement: The native provider reads through one unsafe OS primitive

Canonical standard-library source SHALL define `OsStandardInput` as an ordinary provider that reads
the process standard-input descriptor through one unsafe `Intrinsic` operation returning `bool` and
writing transferred count, low-level reason, and native code to explicit initialized scalar outputs.
A successful zero-length transfer SHALL become `EndOfInput`; a successful positive count SHALL become
`Filled`; and `false` SHALL become `StreamReadError`. No compiler phase MAY construct Option or
recognize the `StandardInput`, `OsStandardInput`, `ReadOutcome`, or `read` spellings to select special
behavior.

#### Scenario: Read through the native implementation

- **WHEN** a provided native implementation receives one exclusive buffer
- **THEN** its source operation invokes one primitive read boundary and preserves the service's outcome and typed failure

#### Scenario: Reject the native read on direct WebAssembly

- **WHEN** a reachable native read is compiled for a direct WebAssembly target
- **THEN** target availability rejects it rather than inventing an input host import

#### Scenario: Link only the reachable runtime symbol

- **WHEN** a native program reads standard input and touches no filesystem
- **THEN** the artifact links the byte-input runtime symbol and no filesystem runtime symbol

### Requirement: Input composition stays above this boundary

This capability SHALL NOT define terminal control, raw mode, color detection, line editing, prompt
handling, buffering policy, decoding, or non-blocking and asynchronous reads. It is not the future
`Stream`/`Sink` model, which will own composition, flow, and backpressure.

#### Scenario: Read without terminal semantics

- **WHEN** a program reads bytes through the service
- **THEN** no terminal mode, prompt, line boundary, or text decoding is invented
