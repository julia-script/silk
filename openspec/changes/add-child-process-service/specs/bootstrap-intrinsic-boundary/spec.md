## ADDED Requirements

### Requirement: Two unsafe child-process primitives are admitted

The sealed `Intrinsic` namespace SHALL expose one unsafe native-only execution operation taking an
executable path, an argument block, an environment block, and a working-directory block as byte
slices plus explicit termination, capture-length, reason, and native-code outputs, and one unsafe
native-only capture operation taking a stream selector, an offset, and an exclusive byte buffer and
returning `Option<usize>`. The argument and environment blocks SHALL be NUL-terminated entry blocks,
and an empty working-directory block SHALL mean the caller's own directory. A successful execution
SHALL retain exactly one capture until the next execution replaces it. The compiler MUST NOT
construct or recognize `ProcessRequest`, `ProcessOutcome`, `ProcessError`, or the `ChildProcess`
service, and MUST NOT admit further operations for shells, streaming, or signal delivery.

#### Scenario: Report a failure to start

- **WHEN** the host cannot start the requested program
- **THEN** the execution operation reports failure and writes the normalized reason and native code without constructing a standard-library value

#### Scenario: Report a nonzero exit code as success

- **WHEN** a child runs to completion and returns a nonzero code
- **THEN** the execution operation succeeds and reports that code as data, leaving the meaning to the library

#### Scenario: Copy one completed capture

- **WHEN** a capture reads the retained result of the immediately preceding execution
- **THEN** it commits the requested prefix into the caller's buffer and reports the exact transferred byte count
