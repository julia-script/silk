## Purpose

Define portable HMAC authentication and HKDF key derivation over SHA-256 and SHA-384, with borrowed inputs and explicit output bounds.

## ADDED Requirements

### Requirement: HMAC exposes concrete consuming streaming actors

`silk/hmac` SHALL expose `HmacSha256` and `HmacSha384`. Each SHALL provide `make(key: &[u8]) -> Self`, `update(self: &mut Self, bytes: &[u8]) -> ()`, consuming `finish(self: Self)`, and `authenticate(key: &[u8], message: &[u8])`. Outputs SHALL be `[u8; 32]` and `[u8; 48]` respectively. Results SHALL implement standardized HMAC, including normalization of keys longer than 64 or 128 bytes respectively. Empty keys, messages, and updates SHALL be admitted. Arbitrary segmentation of the same message SHALL preserve the tag.

#### Scenario: Authenticate a segmented message

- **WHEN** either HMAC actor receives a message in multiple updates, including empty updates and updates crossing its hash block boundary
- **THEN** consuming finalization returns the same fixed-size tag as one-shot authentication

#### Scenario: Normalize a long key

- **WHEN** a key exceeds the selected SHA block size
- **THEN** HMAC uses the hash of the key and produces the standardized tag

#### Scenario: Prevent use after finish

- **WHEN** source attempts to update or finish an HMAC state after consuming finalization
- **THEN** ownership analysis rejects the subsequent use

### Requirement: HKDF extracts fixed-size pseudorandom keys and expands into borrowed output

`silk/hkdf` SHALL expose `HkdfSha256` and `HkdfSha384`. Each SHALL provide `extract(salt: &[u8], ikm: &[u8])` returning `[u8; 32]` or `[u8; 48]`, and `expand(prk: &([u8; 32]), info: &[u8], output: &mut [u8])` or the corresponding 48-byte PRK signature. Expansion SHALL return `Result<(), OutputTooLongError>`. Extract and expand SHALL implement RFC 5869. Empty salt SHALL have the RFC's HashLen-zero-byte salt semantics; empty info and zero output SHALL be valid.

#### Scenario: Derive multiple output blocks

- **WHEN** a valid PRK and info are expanded into an output longer than one digest
- **THEN** the output equals the requested prefix of RFC 5869 expansion, including a partial final block

#### Scenario: Admit empty inputs and output

- **WHEN** salt or info is empty, or output has zero length
- **THEN** extraction and expansion succeed with RFC 5869 semantics

### Requirement: HKDF rejects excess output before mutation

Expansion SHALL admit lengths up to and including `255 * HashLen`: 8160 bytes for SHA-256 and 12240 bytes for SHA-384. Larger lengths SHALL return `OutputTooLongError` with public `requested: usize` and `maximum: usize` fields and SHALL leave every output byte unchanged. Output-length rejection MUST NOT trap or assert.

#### Scenario: Expand the maximum length

- **WHEN** output length is exactly 8160 or 12240 for its respective actor
- **THEN** expansion succeeds through counter byte 255 and writes the correct entire output

#### Scenario: Reject one excess byte

- **WHEN** output length exceeds the maximum by one byte
- **THEN** expansion returns the precise requested and maximum lengths without changing output

### Requirement: HMAC and HKDF remain ordinary portable source

All four actors SHALL compose existing SHA-2 operations in ordinary Silk source with fixed-size state and borrowed input. They MUST NOT require allocation or copies proportional to message, salt, IKM, or info length, compiler privilege, native crypto, providers, or new intrinsic operations. The standard-library manifest, generated surfaces, and prescriptive reference SHALL expose the admitted lifecycle, result sizes, bounds, and scope limitations. No zeroization or tag-verification guarantee SHALL be implied.

#### Scenario: Resolve public operations

- **WHEN** tooling resolves the HMAC and HKDF operations
- **THEN** they resolve to canonical ordinary-source declarations exposed by the manifest and generated documentation

### Requirement: Known-answer evidence covers both selected hashes

Consolidated acceptance evidence SHALL compare HMAC for both hashes against RFC 4231, HKDF-SHA-256 against RFC 5869, and HKDF-SHA-384 against a pinned independent known-answer corpus. It SHALL also cover empty keys/messages, chunking, and both exact maximum and rejected excess output without duplicating compiler pipelines for each vector.

#### Scenario: Execute the shared acceptance corpus

- **WHEN** repository verification executes the HMAC/HKDF corpus program
- **THEN** independent known answers and all distinct boundary classes pass for both hash selections
