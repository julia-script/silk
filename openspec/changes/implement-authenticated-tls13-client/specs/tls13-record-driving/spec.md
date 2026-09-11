## Purpose

Define allocation-free, failure-atomic protected-epoch replacement needed by the authenticated TLS client without weakening record-owner invariants.

## ADDED Requirements

### Requirement: Idle record owners replace epochs in place atomically

`TlsRecordSender.replaceEpoch(&mut self, suite, trafficSecret)` and `TlsRecordReceiver.replaceEpoch(&mut self, suite, trafficSecret)` SHALL return `Result<(), RecordError>`. Each operation SHALL validate owner state and derive all candidate key material before committing any field. Wrong-width secrets, pending sender output, partial or ready receiver input, and terminal receiver state SHALL return the exact record error while preserving the complete prior epoch, sequence, buffers, pending suffix, and receive state. Success SHALL reuse all allocations, install the selected suite key and IV, and reset the sequence to zero. Exhaustion alone SHALL NOT prevent a successful replacement.

#### Scenario: Failure preserves one send epoch

- **WHEN** replacement is attempted while output is pending or with a wrong-width traffic secret
- **THEN** the failure leaves the pending bytes, suite, key, IV, sequence, and record budget unchanged

#### Scenario: Success replaces an exhausted epoch

- **WHEN** an idle protected record owner reaches its final admitted sequence and receives a valid next traffic secret
- **THEN** replacement succeeds without allocation and the next record uses sequence zero under only the new key

### Requirement: Remaining send budget is observable without mutation

`TlsRecordSender.recordsRemaining(&self) -> u64` SHALL return the exact number of additional records admitted in the current protected epoch and zero for plaintext framing. Inspection SHALL neither reserve a sequence nor change pending output.

#### Scenario: Inspect the reserved final slot

- **WHEN** one protected send record remains
- **THEN** repeated inspection returns one until a record is queued or the epoch is replaced
