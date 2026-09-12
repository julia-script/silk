## Purpose

Provide portable IETF ChaCha20-Poly1305 authenticated encryption with bounded memory and explicit rejection that preserves caller destinations.

## ADDED Requirements

### Requirement: Detached authenticated encryption

The library SHALL provide `ChaCha20Poly1305.seal(key, nonce, aad, plaintext, ciphertext, tag)` and `open(key, nonce, aad, ciphertext, tag, plaintext)` in `silk/chacha20_poly1305`. Inputs SHALL be borrowed byte slices; output slices SHALL be exclusive borrows. Both operations SHALL return `Result<(), AeadError>`, require no service, and allocate no heap storage. Keys SHALL contain 32 bytes, nonces 12 bytes, and detached tags 16 bytes. Successful output SHALL equal RFC 8439 AEAD_CHACHA20_POLY1305, with ciphertext length equal to plaintext length; spare destination capacity SHALL remain unchanged.

#### Scenario: Seal and authenticate

- **WHEN** valid inputs are sealed and then opened with the same key, nonce and AAD
- **THEN** ciphertext and tag match independently pinned known answers, and open returns the original plaintext
- **AND** empty plaintext or AAD are valid

### Requirement: Atomic rejection and bounded lengths

Before any destination mutation, operations SHALL reject invalid key/nonce/tag widths, insufficient output capacity, and payload lengths above 274877906880 bytes. AAD length SHALL fit the 64-bit byte-length domain. Size validation SHALL not overflow target `usize`. `AeadError` SHALL distinguish `InvalidKey`, `InvalidNonce`, `InvalidTag`, `OutputTooSmall`, `LimitExceeded`, and `AuthenticationFailed`, in that preflight order. Open SHALL authenticate before plaintext writes. Every failure SHALL preserve all destination bytes, including seal's detached tag.

#### Scenario: Tampering and invalid arguments

- **WHEN** a tag, AAD, nonce or ciphertext is modified, or a preflight condition fails
- **THEN** the operation returns its typed failure without changing output or a detached output tag
- **AND** no input rejection traps or releases unauthenticated plaintext

#### Scenario: Counter and pointer-width limits

- **WHEN** payload length is checked at the algorithm maximum or one greater
- **THEN** the maximum is admitted and the excess rejected without large allocations
- **AND** a wasm32 build performs the same check without narrowing the maximum to usize

### Requirement: Explicit nonce and assurance boundary

Callers SHALL supply a nonce unique under each key and own per-key usage accounting. The operations SHALL not generate nonces, require entropy, or provide TLS record policy. Implementation SHALL use ordinary source, fixed-size private arithmetic, and no secret-dependent addresses, table lookups or variable-work arithmetic. Full tag comparison SHALL use fixed work. Generated-output review SHALL state the inspected targets and limitations; functional vectors SHALL not imply constant time, physical erasure, production security or FIPS validation.

#### Scenario: Portable deterministic use

- **WHEN** the same admitted input is run through LLVM native and LLVM-to-Wasm
- **THEN** both produce the selected known-answer output without external crypto imports
- **AND** documentation separates this primitive from TLS, trust, transport and entropy provision
