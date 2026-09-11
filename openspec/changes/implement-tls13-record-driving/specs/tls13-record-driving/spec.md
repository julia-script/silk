## Purpose

Define bounded TLS 1.3 plaintext and protected record ownership so callers can drive fragmented byte transports without retaining caller buffers, replaying output, or releasing unauthenticated plaintext.

## ADDED Requirements

### Requirement: Record epochs own bounded storage and key state

The standard library SHALL expose separate affine sender and receiver values for AES-128-GCM/SHA-256, AES-256-GCM/SHA-384, and ChaCha20-Poly1305/SHA-256. A protected constructor SHALL require the suite hash's exact 32- or 48-byte traffic secret, reject another length before allocation, derive the AEAD key and 12-byte IV through the delivered TLS HKDF actor, and allocate every reusable buffer once. A distinct plaintext constructor SHALL take no secret and SHALL not fabricate a protected epoch. Sequence and key-generation state SHALL remain private, non-copyable, and non-resettable.

#### Scenario: Reject a mismatched traffic secret atomically

- **WHEN** a protected constructor receives a secret whose width does not match its suite hash
- **THEN** it returns the typed requested and expected lengths without allocating any record storage

#### Scenario: Construct bounded direction owners

- **WHEN** valid protected sender and receiver values are constructed
- **THEN** the sender owns one 16645-byte pending-wire allocation plus one 16385-byte staging allocation required by the detached AEAD contract, and the receiver owns one 16645-byte receive-wire allocation plus one 16385-byte decrypted-record allocation, with no later record-operation allocation

### Requirement: Receive driving consumes only the current record

Input driving SHALL copy only the reported input prefix into owned storage, retain no caller borrow, and report `NeedInput`, `RecordReady`, or zero-consumption `NeedRecordConsumption`. Empty input SHALL be an explicit zero-consumption `NeedInput`, not EOF. A complete current record SHALL stop consumption before any coalesced following record. Record inspection SHALL return an owner-borrowed view, and consumption SHALL be required before input driving can resume.

#### Scenario: Reassemble every byte boundary

- **WHEN** a valid record is supplied one byte at a time or at arbitrary header/body boundaries
- **THEN** each call reports its exact consumed prefix and one identical record becomes ready only after the complete wire record arrives

#### Scenario: Preserve a coalesced following record

- **WHEN** one input slice contains a complete record followed by bytes of another
- **THEN** input driving consumes exactly the first record, reports `RecordReady`, and consumes none of the suffix before the ready record is consumed

#### Scenario: Refuse mutation while a view is live

- **WHEN** a caller retains the borrowed record view
- **THEN** Silk ownership rejects input driving or record consumption through the same receiver

### Requirement: Send driving acknowledges one stable ciphertext

Queueing SHALL accept at most one 16384-byte content fragment and report the exact accepted prefix. A nonempty accepted input SHALL make positive progress; an already-pending record SHALL instead return zero-consumption `NeedOutput`. Pending-output inspection and zero acknowledgment SHALL be idempotent. Partial acknowledgment SHALL expose exactly the unacknowledged suffix, and an acknowledgment beyond that suffix SHALL return `InvalidAcknowledgment` without changing state. No next record SHALL be encrypted and no sequence SHALL be reserved until the pending record is fully acknowledged.

#### Scenario: Drive output one byte at a time

- **WHEN** a queued record is repeatedly inspected and acknowledged in one-byte prefixes
- **THEN** every wire byte appears exactly once in order and repeated inspection never re-encrypts or reserves another nonce

#### Scenario: Reject an oversized acknowledgment

- **WHEN** acknowledgment exceeds the current pending-output length
- **THEN** the sender returns `InvalidAcknowledgment` and the pending bytes and send sequence remain unchanged

### Requirement: TLS record wire semantics are strict and authenticated

Plaintext content SHALL be at most 16384 bytes. Protected outer records SHALL use content type application_data, legacy version 0x0303, authenticate the exact five-byte header as AAD, carry no more than 16640 encrypted payload bytes, and decrypt to no more than 16385 TLSInnerPlaintext bytes. Sending SHALL add a content type byte and no padding. Receiving SHALL accept zero padding only after successful authentication, locate the content type within verified plaintext, reject an all-zero inner plaintext or unknown/illegal inner type, and publish no content before all checks succeed. Empty application content SHALL be legal; empty handshake content SHALL fail; each alert SHALL contain exactly one two-byte alert.

#### Scenario: Reject tampering without plaintext release

- **WHEN** any authenticated outer-header, ciphertext, or tag byte changes
- **THEN** the receiver returns `AuthenticationFailed`, publishes no record view, invokes no consumer, and becomes terminal

#### Scenario: Bound a declared record before its body

- **WHEN** the five-byte header declares a payload beyond its mode's maximum or an impossible protected length
- **THEN** the receiver returns `RecordOverflow` before copying any body byte

#### Scenario: Frame the TLS 1.3-only plaintext ClientHello

- **WHEN** plaintext handshake content is queued
- **THEN** its header uses legacy version 0x0303, while receive parsing tolerates the admitted RFC legacy plaintext versions without negotiating TLS 1.2

### Requirement: Every protected record uses one bounded sequence number

The record nonce SHALL be the static IV XOR the padded big-endian 64-bit sequence. New epochs SHALL begin at zero. Exactly one sequence SHALL be reserved per successfully encoded record and exactly one SHALL be advanced per accepted authenticated record. Every suite SHALL stop before more than 8388608 records use one traffic epoch or before 64-bit sequence wrap. Caller-precondition errors SHALL be atomic and SHALL not poison an epoch; any peer record error SHALL terminally invalidate its receiver.

#### Scenario: Exercise the final admitted sequence

- **WHEN** private fixture construction places a sender or receiver at the last admitted sequence
- **THEN** one final valid record succeeds and the following protected operation returns `KeyUsageExhausted` without a public sequence setter or a million-record loop

#### Scenario: Reject future input after a peer error

- **WHEN** header, authentication, content, or limit validation has terminally rejected a peer record
- **THEN** all later receive driving returns `InvalidState` with zero published content

### Requirement: Verification distinguishes record protection from identity authentication

Verification SHALL include RFC 8448 section 3 exact SHA-256 record/key-schedule bytes and independent pinned AES-256/SHA-384 and ChaCha20-Poly1305/SHA-256 fixtures. It SHALL cover arbitrary fragmentation, coalescing, empty buffers, acknowledgment replay, bounds, tampering, lifetime rejection, native execution, and one LLVM-to-Wasm portability witness at the cheapest tier that falsifies each claim. Public documentation SHALL state that possession of traffic keys does not authenticate a server, and no HTTPS, handshake, network-resource, verification-bypass, or truncation-bypass API SHALL be added by this capability.

#### Scenario: Audit the public boundary

- **WHEN** generated standard-library documentation and manifests are reviewed
- **THEN** the record actors and their bounds are documented while authenticated client and duplex transport responsibilities remain assigned to later capabilities
