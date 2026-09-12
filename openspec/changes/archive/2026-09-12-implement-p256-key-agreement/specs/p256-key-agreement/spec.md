## Purpose

Provide bounded ordinary-source P-256 key agreement with strict scalar and peer admission, explicit fresh entropy, and fixed-width interoperable public and shared values.

## ADDED Requirements

### Requirement: Scalar ownership and admission

The standard library SHALL expose an opaque move-only P256 scalar actor. `fromBytes` SHALL borrow exactly 32 big-endian bytes, admit integers 1 through n−1, and return typed InvalidLength or InvalidScalar failures otherwise. Public errors SHALL contain no secret bytes. No allocator or provider SHALL be required for deterministic operations.

#### Scenario: Scalar boundaries

- **WHEN** scalar zero, n, n−1, and one are imported
- **THEN** zero and n fail and n−1 and one succeed without changing input

### Requirement: Public key and agreement

`publicKey` SHALL borrow an admitted scalar and return 65 bytes 0x04 || x || y. `agree` SHALL consume the scalar, borrow exactly 65 peer bytes, and return the fixed 32-byte big-endian shared x-coordinate. Peer admission SHALL reject wrong size, wrong prefix, out-of-field coordinates, infinity and off-curve points with typed InvalidLength, InvalidEncoding or InvalidPoint errors. No failed call SHALL return a shared secret. Leading zero coordinate bytes SHALL be retained. Compressed points SHALL NOT be admitted.

#### Scenario: Independent peer agreement

- **WHEN** the RFC 5903 section 8.1 private scalars and SEC1-adapted public points are used
- **THEN** both directions return the specified shared x-coordinate and public-key derivation returns the specified coordinates

#### Scenario: Invalid peer

- **WHEN** a peer has a wrong prefix, noncanonical coordinate, infinity encoding or off-curve coordinates
- **THEN** agreement returns the matching admission error without exposing a secret

#### Scenario: Consumed ephemeral key

- **WHEN** source attempts to agree twice with the same scalar owner
- **THEN** ordinary ownership analysis rejects its second use

### Requirement: Explicit generation

`generate` SHALL require exclusive Random and repeatedly request 32 fresh bytes until a big-endian integer in 1..n−1 is obtained. Generation SHALL use unbiased rejection sampling, preserve Random's fatal failure policy, and never select InsecureRandom or an ambient provider. Each invocation SHALL acquire fresh entropy, without promising distinct outcomes for coincident random draws.

#### Scenario: Reject scripted out-of-range draws

- **WHEN** a test provider supplies zero, n, then one
- **THEN** generation consumes three complete requests and returns the scalar whose public key is the standard generator

### Requirement: Bounded portable arithmetic and honest assurance

The implementation SHALL use ordinary source, bounded inline storage, no crypto-specific intrinsic, no external crypto fallback and no public bigint framework. Secret multiplication SHALL use fixed schedules without secret-dependent branches, addresses or lookup tables. Scalar admission/generation rejection SHALL remain distinct from that arithmetic guarantee. Native and LLVM-to-Wasm deterministic evidence SHALL use pinned independent vectors. Generated-output inspection SHALL document its target and optimization scope; passing vectors SHALL NOT claim constant-time proof, secure erasure, FIPS validation or production security.

#### Scenario: Portable deterministic operation

- **WHEN** an admitted deterministic agreement is compiled through LLVM for native and Wasm
- **THEN** both return the committed expected bytes without an entropy host import
