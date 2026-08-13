## ADDED Requirements

### Requirement: Four unsafe process-input primitives are admitted

The sealed `Intrinsic` namespace SHALL expose four unsafe native-only process-input operations: an
argument count with an exclusive `usize` output returning `bool`, and argument, environment-value,
and working-directory lookups each taking an exclusive byte buffer plus explicit reason and
native-code outputs and returning `Option<usize>`. A present count SHALL be the value's complete byte
length with the prefix that fits copied into the buffer; an absent result SHALL write the normalized
low-level reason and native code, where the not-found reason means the value does not exist. The
compiler MUST NOT construct or recognize `HostInputFailure` or the `HostInput` service, and MUST NOT
admit an operation that sets an environment variable, changes the working directory, or parses
arguments.

#### Scenario: Report a value longer than the buffer

- **WHEN** the host holds a value longer than the buffer the caller supplied
- **THEN** the intrinsic copies the prefix that fits and reports the complete byte length, without a separate buffer-too-small protocol

#### Scenario: Report an absent value

- **WHEN** an argument index is past the last argument or an environment name is unset
- **THEN** the intrinsic returns `None` with the not-found reason and the library decides what that means

#### Scenario: Report a refused lookup

- **WHEN** the host cannot answer a lookup at all
- **THEN** the intrinsic reports the normalized reason and native code without constructing a standard-library value
