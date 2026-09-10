## ADDED Requirements

### Requirement: File-open metadata outcomes preserve their cause

After successful metadata inspection identifies a nonregular object, file-open SHALL report
WrongType with no native error, regardless of residual native error state. Failed metadata
inspection SHALL report its actual native error captured before cleanup. Both rejected-open
outcomes SHALL release the acquired descriptor exactly once and create no handle; cleanup failure
MUST NOT replace the primary outcome.

#### Scenario: Nonregular metadata with stale native error

- **WHEN** metadata inspection succeeds with a nonregular mode while a previous native error remains set
- **THEN** file-open reports WrongType without a native error and closes the acquired descriptor exactly once

#### Scenario: Metadata failure followed by cleanup failure

- **WHEN** metadata inspection fails and closing the acquired descriptor changes the native error state
- **THEN** file-open reports the metadata call's original native error and attempts no second close
