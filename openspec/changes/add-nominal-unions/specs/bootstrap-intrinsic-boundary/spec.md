## ADDED Requirements

### Requirement: Recoverable primitives are carrier-neutral

No intrinsic contract SHALL name, construct, match, or recognize source-defined `Option`, `Result`,
or their variants. Existing checked scalar primitives SHALL receive ordinary present and absent
carrier inputs and return their shared result type. The inventory, semantic analysis, HIR, MIR,
evaluation, and every backend SHALL treat those carriers through their ordinary exact callable and
value contracts. Completed Effect outcomes SHALL be handled by ordinary Effect composition rather
than an intrinsic. This change SHALL replace the abstraction-shaped existing signatures, SHALL remove
`Intrinsic.effectResult` and all of its compiler support, and SHALL add no replacement source-callable
operation.

#### Scenario: Construct Option in an integer wrapper

- **WHEN** an ordinary checked-integer wrapper supplies the ordinary `some<T>` and `none<T>` constructor functions to its scalar primitive
- **THEN** the primitive selects the correct ordinary carrier and contains no canonical Option or variant identity

#### Scenario: Keep completed Effect reification out of Intrinsic

- **WHEN** ordinary `Effect.result` maps success and catches the complete typed failure in library code
- **THEN** the intrinsic inventory contains no completed-outcome operation and the compiler contains no dedicated HIR, MIR, evaluator, or backend path for it

#### Scenario: Audit the closed inventory

- **WHEN** the intrinsic inventory is compared before and after migration
- **THEN** abstraction-shaped Option and Result result contracts are gone, `Intrinsic.effectResult` is absent, no replacement callable operation exists, and every remaining changed primitive has one carrier-neutral contract

## MODIFIED Requirements

### Requirement: OS filesystem privilege is handle-level and sealed

The `Intrinsic` namespace SHALL contain only the unsafe file open/read/write, directory open/next,
path inspection, directory creation, file removal, directory removal, and generic consuming close
operations required to build an OS provider. Their signatures SHALL use primitive scalars, slices,
explicit scalar output parameters, `bool`, exact `once fn` carriers, and opaque `OsHandle`.
Handle-producing opens SHALL transfer a newly initialized handle only as the argument of the selected
success carrier and SHALL select a zero-argument failure carrier after writing reason outputs; they
MUST NOT require an optionally initialized handle place. Count-producing operations SHALL return
`bool` and write counts and failure details to initialized scalar outputs. No operation may use or
construct a source-defined optional carrier, filesystem service, or domain value.

#### Scenario: Build a source provider from low-level calls

- **WHEN** canonical `OsFileSystem` implements a whole-file read
- **THEN** it composes open, repeated read, and consuming close rather than invoking a compiler-known whole-file operation

#### Scenario: Keep portable operations ordinary

- **WHEN** another source-defined provider implements `FileSystem.readFile`
- **THEN** it can satisfy the service without invoking any OS intrinsic or receiving name-based compiler treatment

#### Scenario: Transfer one opened handle through a carrier

- **WHEN** a file or directory open succeeds
- **THEN** the intrinsic invokes the success carrier exactly once with the newly initialized affine `OsHandle`, cleans the unused failure carrier, and transfers one close obligation without an optional output place

#### Scenario: Refuse an open without initializing a handle

- **WHEN** a file or directory open fails
- **THEN** the intrinsic writes the normalized reason outputs, invokes the failure carrier exactly once, cleans the unused success carrier, and creates no `OsHandle` or close obligation

### Requirement: One unsafe byte-input primitive is admitted

The sealed `Intrinsic` namespace SHALL expose one unsafe native-only byte-input operation taking an
exclusive byte buffer plus exclusive transferred-count, reason, and native-code outputs and returning
`bool`. Success SHALL write the exact transferred byte count, including zero for end of input; failure
SHALL write the normalized low-level reason and native code. The compiler MUST NOT construct or
recognize an optional carrier, `ReadOutcome`, `StreamReadError`, or the `StandardInput` service, and
MUST NOT admit a second input operation for buffering, decoding, or terminal control.

#### Scenario: Report a refused read

- **WHEN** the host refuses a standard-input read
- **THEN** the intrinsic returns `false` and writes the normalized reason and native code without constructing a standard-library value

#### Scenario: Report the end of input

- **WHEN** the host reports that no further bytes will arrive
- **THEN** the intrinsic returns `true` with a zero count and the library decides what that means

### Requirement: Two unsafe child-process primitives are admitted

The sealed `Intrinsic` namespace SHALL expose one unsafe native-only execution operation taking an
executable path, an argument block, an environment block, and a working-directory block as byte
slices plus explicit termination, capture-length, reason, and native-code outputs, and one unsafe
native-only capture operation taking a stream selector, an offset, an exclusive byte buffer, and
exclusive transferred-count and reason outputs and returning `bool`. The argument and environment
blocks SHALL be NUL-terminated entry blocks, and an empty working-directory block SHALL mean the
caller's own directory. A successful execution SHALL retain exactly one capture until the next
execution replaces it. The compiler MUST NOT construct or recognize an optional carrier,
`ProcessRequest`, `ProcessOutcome`, `ProcessError`, or the `ChildProcess` service, and MUST NOT admit
further operations for shells, streaming, or signal delivery.

#### Scenario: Report a failure to start

- **WHEN** the host cannot start the requested program
- **THEN** the execution operation reports failure and writes the normalized reason and native code without constructing a standard-library value

#### Scenario: Report a nonzero exit code as success

- **WHEN** a child runs to completion and returns a nonzero code
- **THEN** the execution operation succeeds and reports that code as data, leaving the meaning to the library

#### Scenario: Copy one completed capture

- **WHEN** a capture reads the retained result of the immediately preceding execution
- **THEN** it returns `true`, commits the requested prefix into the caller's buffer, and writes the exact transferred byte count

### Requirement: Four unsafe process-input primitives are admitted

The sealed `Intrinsic` namespace SHALL expose four unsafe native-only process-input operations: an
argument count with an exclusive `usize` output returning `bool`, and argument, environment-value,
and working-directory lookups each taking an exclusive byte buffer plus exclusive complete-length,
reason, and native-code outputs and returning `bool`. Success SHALL write the value's complete byte
length with the prefix that fits copied into the buffer; failure SHALL write the normalized low-level
reason and native code, where the not-found reason means the value does not exist. The compiler MUST
NOT construct or recognize an optional carrier, `HostInputError`, or the `HostInput` service, and MUST
NOT admit an operation that sets an environment variable, changes the working directory, or parses
arguments.

#### Scenario: Report a value longer than the buffer

- **WHEN** the host holds a value longer than the buffer the caller supplied
- **THEN** the intrinsic returns `true`, copies the prefix that fits, and writes the complete byte length without a separate buffer-too-small protocol

#### Scenario: Report an absent value

- **WHEN** an argument index is past the last argument or an environment name is unset
- **THEN** the intrinsic returns `false` with the not-found reason and the library decides what that means

#### Scenario: Report a refused lookup

- **WHEN** the host refuses an otherwise valid process-input lookup
- **THEN** the intrinsic returns `false` with the normalized non-not-found reason and native code
