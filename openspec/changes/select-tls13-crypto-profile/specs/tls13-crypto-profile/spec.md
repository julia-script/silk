## Purpose

Define the intended cryptographic interoperability and security boundaries for a bounded TLS 1.3 client and its certificate consumers, without claiming these future algorithms are shipped.

## ADDED Requirements

### Requirement: The first profile has explicit independent algorithm sets

The initial profile SHALL support a TLS 1.3 server-authenticated client with fresh ephemeral ECDHE and SHALL require all entries in the design's algorithm matrix: AES-128-GCM/SHA-256, AES-256-GCM/SHA-384 and ChaCha20-Poly1305/SHA-256 suites; P-256 and X25519 agreement; ECDSA-P256/SHA-256 and RSA-PSS-RSAE/SHA-256 server CertificateVerify; and those signature algorithms plus RSA-PKCS1-v1_5/SHA-256 for certificates. No optional runtime algorithms are selected. Suite hashes SHALL NOT constrain or substitute for signature hashes. Older TLS versions, PSK/resumption, 0-RTT and client authentication SHALL be unavailable in this profile. Algorithm support SHALL NOT imply complete protocol implementation or general Web PKI compatibility.

#### Scenario: Select algorithms independently

- **WHEN** an admitted server selects AES-256-GCM/SHA-384 with an admitted SHA-256 signature and either admitted group
- **THEN** the cryptographic profile admits the combination without requiring SHA-384 signature support

#### Scenario: Reject a certificate-only handshake algorithm

- **WHEN** a peer attempts PKCS#1 v1.5 for TLS CertificateVerify
- **THEN** the consumer rejects the unsupported scheme even though certificate verification supports it

#### Scenario: Encounter an excluded certificate signature

- **WHEN** an otherwise usable path requires SHA-384, SHA-1, EdDSA or an excluded curve/key algorithm for a signature that must be verified
- **THEN** the profile rejects that signature explicitly instead of using a platform fallback

### Requirement: Cryptographic inputs have bounded policy and typed failures

The future primitive APIs SHALL enforce the design's key/parameter matrix before expensive work or output mutation. AEAD SHALL use 96-bit nonces and 128-bit tags, with 128/256-bit AES keys or a 256-bit ChaCha key. Curve agreement SHALL validate P-256 points/scalars and apply RFC 7748 X25519 decoding with all-zero shared-secret rejection. RSA SHALL accept only odd 2048..4096-bit moduli and exponent 65537 with rsaEncryption public keys. PSS SHALL use SHA-256, MGF1-SHA-256, 32-byte salt and trailer field 1. ECDSA SHALL admit only P-256/SHA-256 with strict DER and r,s in 1..n−1, including valid high-s signatures. PKCS#1 verification SHALL enforce complete padding and DigestInfo encoding. Unsupported algorithms, malformed keys/parameters/encodings, invalid lengths, inadequate outputs and exhausted limits SHALL return semantic typed errors without secret details. No signature success SHALL mean certificate trust, identity match or path validity.

#### Scenario: Validate before public exponentiation

- **WHEN** an RSA key exceeds 4096 bits or its exponent differs from 65537
- **THEN** verification returns a policy error without entering unbounded arithmetic

#### Scenario: Reject a PSS parameter mismatch

- **WHEN** a PSS signature requests a different MGF1 hash or salt length
- **THEN** verification rejects the parameters even if a permissive library could verify the signature

#### Scenario: Validate key agreement peers

- **WHEN** P-256 receives an invalid curve point or X25519 computes an all-zero result
- **THEN** agreement returns a typed key/agreement failure and exposes no shared secret

### Requirement: Authenticated decryption never publishes unauthenticated plaintext

AEAD open SHALL leave caller-owned output unchanged on any rejection, including authentication failure. Successful open SHALL authenticate ciphertext, nonce and AAD before exposing plaintext. Tags SHALL be detached from ciphertext. AES-GCM SHALL limit plaintext/ciphertext to `2^36 − 32` bytes and AAD to `2^61 − 1` bytes. ChaCha20-Poly1305 SHALL limit plaintext/ciphertext to `64 × (2^32 − 1)` bytes and AAD to `2^64 − 1` bytes, subject also to addressable slice and output capacity. Length and counter limits SHALL be checked without target-integer overflow before mutation. Nonce uniqueness and per-key usage accounting SHALL be explicit caller obligations; the primitive SHALL NOT generate random nonces or silently wrap a counter. Input and output borrows SHALL have explicit non-aliasing ownership.

#### Scenario: Reject a modified authentication input

- **WHEN** the ciphertext, AAD, nonce or tag is modified and authentication fails
- **THEN** open returns AuthenticationFailed and preserves the complete destination

### Requirement: Target and provider availability is honest and explicit

New crypto and label operations SHALL be ordinary source and SHALL NOT depend on compiler-known library spelling, crypto-specific intrinsics or hidden external crypto fallbacks. Ephemeral generation SHALL require explicit production Random provision and retain its fatal provider-failure contract. The initial entropy matrix SHALL be Darwin ARM64/system libc and GNU/Linux x86-64 or ARM64/GNU libc only. LLVM-to-Wasm SHALL have no implied production entropy provider; unsupported native profiles SHALL have no implied complete-profile support. Deterministic test providers SHALL NOT satisfy a production-security claim. The retired runtime evaluator and independent direct-Wasm backend SHALL NOT be test or support targets. Current new primitives, labels and end-to-end TLS SHALL be documented as absent until separately delivered.

#### Scenario: Compile without a production entropy provider

- **WHEN** a portable program targets LLVM-to-Wasm and attempts to use the official native entropy member
- **THEN** source selection rejects the unavailable member and does not substitute deterministic or ambient host entropy

#### Scenario: Fail secure generation

- **WHEN** a selected Random provider cannot fill a requested secret
- **THEN** generation does not return weak/partial success or recover through InsecureRandom

### Requirement: Implementation evidence separates functional correctness from security assurance

Each selected primitive and label operation SHALL have the authoritative known-answer mapping and immutable independent reference pin recorded in evidence.md. Negative cases SHALL distinguish malformed encoding, invalid parameters and failed authentication. Selected native and LLVM-to-Wasm evidence SHALL use the cheapest adequate tiers without an independent direct-Wasm or runtime-evaluator leg. Implementations SHALL document secret-dependent work and inspect selected optimized output; vectors alone SHALL NOT imply constant-time execution, formal verification, FIPS validation or secure erasure. No compiler-copy erasure guarantee SHALL be advertised.

#### Scenario: Review a primitive for admission

- **WHEN** an implementation requests a supported-profile claim
- **THEN** its review includes pinned functional evidence, target results, parameter rejection and side-channel limitations rather than relying on successful vectors alone
