## Purpose

Provide bounded deterministic ECDSA P-256/SHA-256 verification of messages and retained certificate metadata with strict encodings and explicit failures.

## ADDED Requirements

### Requirement: Borrowed message verification

The standard library SHALL borrow a message, uncompressed SEC1 P-256 public key and DER signature and return typed success or failure. It SHALL hash the message exactly once with SHA-256, reject byte lengths exceeding 2^61−1 before hashing, and require no allocator, entropy or runtime provider. It SHALL NOT expose a prehashed verification operation as part of this capability. Invalid keys SHALL reject wrong size or prefix, noncanonical coordinates, infinity and off-curve points.

#### Scenario: Independent message signature

- **WHEN** an independently specified valid P-256/SHA-256 signature and its message and public key are supplied
- **THEN** verification succeeds without changing or consuming the borrowed bytes

#### Scenario: Message mutation

- **WHEN** the message of an otherwise valid signature is changed
- **THEN** verification returns AuthenticationFailed

### Requirement: Strict signature encoding

The verifier SHALL accept exactly one DER SEQUENCE containing exactly two minimally encoded positive INTEGERs r and s in 1..n−1. It SHALL reject trailing bytes, truncation, negative values, redundant sign padding, indefinite or overlong lengths, zero and values at least n as InvalidEncoding. Both high-s and low-s signatures SHALL be admitted.

#### Scenario: Equivalent high and low s

- **WHEN** a valid signature's s is replaced by n−s with canonical DER encoding
- **THEN** both signatures verify successfully

#### Scenario: Noncanonical DER

- **WHEN** a signature adds an unnecessary INTEGER sign byte or uses an overlong length
- **THEN** verification returns InvalidEncoding

### Requirement: Complete verification equation

Verification SHALL handle a reduced digest of zero and zero scalar terms correctly. It SHALL reject a final verification point at infinity and SHALL compare r against the final affine x-coordinate reduced modulo n. A mathematically invalid signature with admitted input encodings SHALL return AuthenticationFailed.

#### Scenario: Zero reduced digest

- **WHEN** the internal verification equation receives a zero reduced digest and otherwise valid signature
- **THEN** the zero generator term remains valid and the equation can succeed

#### Scenario: Infinite verification result

- **WHEN** the two verification terms sum to the identity
- **THEN** verification returns AuthenticationFailed

### Requirement: Retained certificate metadata policy

The metadata adapter SHALL accept borrowed AlgorithmView and BitStringView values without requiring a Certificate handle. It SHALL require id-ecPublicKey with exact named secp256r1 parameters, ecdsa-with-SHA256 with absent parameters, and zero unused bits in key and signature. It SHALL reject id-ecDH, id-ecMQV and other algorithms as UnsupportedAlgorithm, and explicit/implicit curves, wrong curves, NULL signature parameters or nonzero unused bits as InvalidParameters. It SHALL perform no certificate extraction, trust-path, identity or signing policy.

#### Scenario: Supported retained metadata

- **WHEN** retained metadata names id-ecPublicKey and secp256r1 and ecdsa-with-SHA256 with absent parameters, and both bit strings have zero unused bits
- **THEN** the adapter verifies their bytes using the message verifier

#### Scenario: Forbidden signature parameters

- **WHEN** ecdsa-with-SHA256 has NULL parameters
- **THEN** the adapter returns InvalidParameters before signature verification

### Requirement: Source ownership and assurance limits

The implementation SHALL reuse the existing private P-256 arithmetic in ordinary source, bounded inline storage and no crypto-specific intrinsic or host fallback. It SHALL preserve existing secret agreement arithmetic's fixed schedules. Native and LLVM-to-Wasm evidence SHALL use pinned independent fixtures and documented oracle limitations. Passing tests SHALL NOT claim a constant-time proof, FIPS validation, physical erasure or production security assurance.

#### Scenario: Portable verification

- **WHEN** a valid deterministic message verification is compiled through LLVM for native and Wasm
- **THEN** both succeed without cryptographic host imports
