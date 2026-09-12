# bounded-rsa-verification Specification

## Purpose

Provide bounded RSA SHA-256 signature verification with strict key, algorithm and encoded-message
admission, independently of certificate container parsing and trust policy.

## Requirements

### Requirement: Public keys admit the selected bounded RSA profile

Construction SHALL admit canonical positive odd 2048..4096-bit moduli, including non-byte-aligned
widths, and exponent 65537. Certificate keys SHALL use rsaEncryption with NULL parameters and a
byte-aligned RSAPublicKey payload containing exactly two minimal positive DER INTEGERs. Rejected
keys SHALL return a typed failure before public exponentiation. Admitted keys SHALL own bounded
storage and SHALL NOT retain input bytes.

#### Scenario: Admit the modulus endpoints and a non-byte-aligned key

- **WHEN** an odd 2048-, 2049- or 4096-bit modulus and exponent 65537 are supplied
- **THEN** construction succeeds and retains the exact modulus bit length

#### Scenario: Reject unsupported keys

- **WHEN** a modulus is even, outside 2048..4096 bits, or has malformed canonical encoding, or the exponent differs from 65537
- **THEN** construction fails before exponentiation

### Requirement: Signature admission precedes bounded verification

Signature byte length SHALL equal ceil(modulusBits/8), and its unsigned representative SHALL be
less than the modulus. Verification SHALL hash the borrowed message exactly once with SHA-256.
Messages longer than 2^61−1 bytes SHALL fail before hashing. No operation SHALL expose signing,
private RSA, entropy, public bigint operations, or a trust/path/identity decision.

#### Scenario: Reject an invalid representative

- **WHEN** signature width differs from the key width or its integer is at least the modulus
- **THEN** verification returns a typed failure before exponentiation

### Requirement: PSS checks the complete selected encoded message

PSS SHALL use SHA-256, MGF1-SHA-256, salt length 32, trailer 1 and emBits=modulusBits−1. Verification
SHALL check every zero padding byte, the delimiter, salt, hash, trailer and leading-bit constraints.
Conversion to emLen SHALL reject a representative that does not fit, including the emLen+1
signature-width case for a 2049-bit modulus.

#### Scenario: Reject early padding corruption

- **WHEN** any padding byte before the last padding byte is nonzero after unmasking
- **THEN** PSS verification fails even when the delimiter and last padding byte are valid

#### Scenario: Preserve non-byte-aligned modulus semantics

- **WHEN** a valid 2049-bit key supplies a valid PSS signature whose recovered encoding occupies 256 bytes
- **THEN** verification succeeds, while a recovered integer requiring 257 bytes fails

### Requirement: Certificate algorithm parameters preserve RFC defaults and equivalence

Certificate verification SHALL require byte-aligned signature payloads. sha256WithRSAEncryption
parameters SHALL be absent or NULL. PSS parameters SHALL be present and structurally valid, ordered
and unique, with no trailing data. Omitted fields SHALL receive RFC 4055 defaults. Effective hash
and MGF hash SHALL be SHA-256, salt length SHALL be 32 and trailer SHALL be 1. Absent and explicit
trailer1 SHALL both succeed. SHA-256 parameters SHALL accept absence or NULL, including the nested
MGF1 hash identifier. PSS-restricted key OIDs SHALL be rejected.

#### Scenario: Admit equivalent parameter spellings

- **WHEN** selected PSS parameters use absent or NULL SHA-256 parameters and absent or explicit trailer1
- **THEN** the equivalent encodings select the same verification profile

#### Scenario: Reject default SHA-1 and malformed fields

- **WHEN** omitted fields imply SHA-1, MGF1-SHA-1 or salt20, or fields are duplicated, out of order, malformed or followed by extra DER
- **THEN** parameter admission fails before signature verification

### Requirement: PKCS1 v1.5 is strict certificate verification

PKCS#1 v1.5 SHA-256 SHALL be available through certificate signature verification only. The entire
encoded message SHALL contain the exact 00 01 prefix, at least eight FF bytes, one delimiter and
exact SHA-256 DigestInfo DER with NULL parameters. BER forms, trailing bytes, short padding,
non-FF padding and partial digest matches SHALL fail.

#### Scenario: Reject a digest prefix match

- **WHEN** an encoded message contains only a prefix of the expected digest, extra bytes, or noncanonical DigestInfo
- **THEN** verification fails despite any matching digest prefix

### Requirement: Evidence is independent and target claims are bounded

Committed evidence SHALL include selected NIST RSA signature vectors, source/member/case/checksum,
reproduction and the pinned Zig revision, plus standards-derived negative encodings. Runtime success
and rejection SHALL use the shared native corpus; a small LLVM-to-Wasm witness SHALL cover intended
portability. Generated target inspection SHALL retain the verifier's reachable code. No result
SHALL claim certificate trust, private-key security, production security certification or targets
that were not validated.

#### Scenario: Audit a fixture and target claim

- **WHEN** a reviewer checks an expected signature result or supported emitted target
- **THEN** the committed evidence identifies its immutable origin, exact reproduction and validation limits
