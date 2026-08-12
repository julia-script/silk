## ADDED Requirements

### Requirement: One unsafe byte-input primitive is admitted

The sealed `Intrinsic` namespace SHALL expose one unsafe native-only byte-input operation taking an
exclusive byte buffer plus explicit reason and native-code outputs and returning `Option<usize>`. A
present count SHALL be the exact transferred byte count, zero SHALL mean the end of input, and an
absent result SHALL write the normalized low-level reason and native code. The compiler MUST NOT
construct or recognize `ReadOutcome`, `StreamReadFailure`, or the `StandardInput` service, and MUST
NOT admit a second input operation for buffering, decoding, or terminal control.

#### Scenario: Report a refused read

- **WHEN** the host refuses a standard-input read
- **THEN** the intrinsic returns `None` and writes the normalized reason and native code without constructing a standard-library value

#### Scenario: Report the end of input

- **WHEN** the host reports that no further bytes will arrive
- **THEN** the intrinsic returns a zero count rather than a failure, and the library decides what that means
