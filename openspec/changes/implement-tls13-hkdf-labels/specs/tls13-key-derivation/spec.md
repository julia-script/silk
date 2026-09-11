## Purpose

Define bounded TLS 1.3 label expansion and secret derivation over delivered generic HKDF, with byte-exact domain separation and failure-before-mutation contracts.

## ADDED Requirements

### Requirement: Label expansion has exact byte framing

Concrete SHA-256 and SHA-384 TLS expansion operations SHALL borrow fixed HashLen secrets, raw label bytes and context bytes and fill caller-owned output via the matching generic HKDF. The info encoding SHALL be `u16be(output.length) || u8(6 + label.length) || ASCII("tls13 ") || label || u8(context.length) || context`. There SHALL be no terminator, string normalization, trimming or automatic recognition/removal of a caller-supplied prefix. Generic HMAC/HKDF SHALL remain separately owned by JUL-162.

#### Scenario: Encode a traffic key request

- **WHEN** SHA-256 expansion receives label `key`, empty context and 16 output bytes
- **THEN** HKDF info is exactly `00 10 09 74 6c 73 31 33 20 6b 65 79 00`

#### Scenario: Preserve caller bytes

- **WHEN** raw label or context contains a zero byte or the raw label begins with `tls13 `
- **THEN** framing preserves every byte and adds the operation's own fixed prefix without special interpretation

### Requirement: Length failures precede output mutation

Raw label lengths SHALL be 1..249 bytes, context lengths 0..255, and output lengths 0..8160 for SHA-256 or 0..12240 for SHA-384. Length validation SHALL precede narrowing, framing, writes and zero-output fast paths. Rejected inputs SHALL return LabelLength, ContextLength or OutputLength with public requested/maximum bounds and SHALL leave output unchanged. Fixed secret/hash widths SHALL prevent selecting the wrong digest width. Error data SHALL NOT contain secret bytes.

#### Scenario: Admit exact boundaries

- **WHEN** label and context are at their maximum lengths and the output is within its hash-specific maximum
- **THEN** the operation succeeds with exact framing and no encoded-length truncation

#### Scenario: Reject invalid framing despite empty output

- **WHEN** label is empty or 250 bytes, or context is 256 bytes, while output is empty
- **THEN** expansion returns its corresponding typed length error rather than bypassing validation

#### Scenario: Reject one excess output byte

- **WHEN** SHA-256 requests 8161 bytes or SHA-384 requests 12241 bytes
- **THEN** expansion returns OutputLength without changing any destination byte

### Requirement: Secret derivation distinguishes messages from transcript hashes

Derive-Secret SHALL return HashLen output from label expansion using Hash(messages) as context. The messages operation SHALL hash exactly the supplied handshake byte sequence; a distinct from-hash operation SHALL accept exactly HashLen transcript bytes and SHALL NOT hash them again. Empty messages SHALL use Hash(empty), not an empty context. SHA-256 input beyond its byte-aligned hash domain SHALL return MessageTooLong before hashing rather than exposing a hash-overflow trap. Transcript serialization, selection and HelloRetryRequest rewriting SHALL belong to the TLS consumer, not these operations. Borrowed inputs SHALL not be retained; label framing SHALL require no storage proportional to transcript size.

#### Scenario: Derive from an empty transcript

- **WHEN** Derive-Secret receives no message bytes
- **THEN** expansion uses the selected hash's digest of the empty byte sequence as context

#### Scenario: Reuse an existing transcript hash

- **WHEN** from-hash derivation receives the digest of the same messages used by messages derivation
- **THEN** both operations return identical secrets without hashing the digest as if it were a message

### Requirement: Label evidence includes independent outputs and distinct framing failures

Verification SHALL compare SHA-256 TLS derivation to RFC 8448 intermediate values and both hash variants to the pinned independent implementation in evidence.md. SHA-384 reference-generated data SHALL be labeled as such, never attributed to nonexistent RFC 5869 or RFC 8448 SHA-384 examples. Evidence SHALL cover exact info encoding, transcript versus empty context, hash selection and the distinct rejection boundaries. The generic HKDF vector corpus SHALL NOT be duplicated in this module's tests.

#### Scenario: Audit fixture provenance

- **WHEN** label fixtures are reviewed
- **THEN** each records its inputs, expected framing/output, authoritative or reference-generated origin and immutable reference revision
