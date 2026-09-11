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

### Requirement: A ready record can be inspected and copied without retaining a view

`TlsRecordReceiver.readyContentType(&self) -> Option<ContentType>` and `readyLength(&self) -> usize` SHALL inspect only the current authenticated ready record. `TlsRecordReceiver.consumeRecordInto(&mut self, destination: &mut [u8]) -> Result<usize, RecordError>` SHALL copy the complete ready plaintext, consume it only after the copy succeeds, and return its exact length. No ready record SHALL return `InvalidState`; insufficient destination capacity SHALL return `RecordOverflow` and preserve the complete record, epoch, and sequence state. These operations exist so an owner such as `Client` can move authenticated bytes into its own bounded storage without retaining a receiver-tied `RecordView`.

#### Scenario: A failed copy preserves the ready record

- **WHEN** a destination is one byte smaller than the authenticated ready content
- **THEN** `RecordOverflow` reports the requested and available sizes and a later adequate copy returns the same complete content
