## Purpose

Provide bounded portable AES-GCM operations for callers that need authenticated ciphertext with explicit nonces, associated data and detached tags.

## ADDED Requirements

### Requirement: Detached authenticated encryption

The standard library SHALL expose ordinary-source AES-GCM seal and open operations admitting 16-byte or 32-byte keys, 12-byte nonces and 16-byte detached tags. Ciphertext SHALL have the plaintext length. Empty plaintext and AAD SHALL be valid. Inputs SHALL be borrowed and destinations exclusively borrowed without aliasing.

#### Scenario: Encrypt and authenticate with either key size

- **WHEN** a caller seals an admitted message and then opens the ciphertext with the same key, nonce, AAD and tag
- **THEN** ciphertext/tag match independent known answers and open returns the original plaintext

### Requirement: Rejection preserves destinations

Every wrong key/nonce/tag width, insufficient output capacity, algorithm limit violation or authentication failure SHALL return a distinct semantic typed error before changing any caller-owned output or tag. Open SHALL authenticate before publishing plaintext. Successful operations SHALL preserve any unused output suffix.

#### Scenario: Reject modified authenticated input

- **WHEN** a tag, nonce, AAD or ciphertext byte is changed and authentication fails
- **THEN** open reports authentication failure and the whole plaintext destination retains its prior bytes

#### Scenario: Reject invalid admission

- **WHEN** any input width, output capacity or message limit is invalid
- **THEN** seal/open returns the corresponding typed error with every destination unchanged

### Requirement: Bounded length and source execution

Plaintext/ciphertext SHALL be limited to 2^36−32 bytes, AAD to 2^61−1 bytes, and both further limited by target addressability. Admission SHALL avoid target-integer overflow before writes. The implementation SHALL use bounded storage, table-free AES, fixed-schedule GHASH and full-tag comparison, with no compiler-known library spelling, crypto intrinsic or external fallback.

#### Scenario: Admit portable limits

- **WHEN** operation lengths are checked on native or wasm32 targets
- **THEN** comparisons preserve the algorithm bounds without truncation, overflow or counter reuse

### Requirement: Explicit security ownership

The caller SHALL provide nonce uniqueness and per-key usage accounting. Operations SHALL NOT generate nonces or require entropy. Documentation SHALL distinguish functional evidence from side-channel assurance and SHALL NOT promise secure erasure or production security from vector success.

#### Scenario: Use a deterministic portable primitive

- **WHEN** a caller invokes an operation on an admitted target
- **THEN** no random provider or external cryptographic service is requested and the same inputs produce the same result
