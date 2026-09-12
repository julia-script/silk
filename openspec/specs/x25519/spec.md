# x25519 Specification

## Purpose

Provide portable X25519 public-key derivation and ephemeral shared-secret agreement with explicit fresh entropy and bounded private arithmetic.

## Requirements

### Requirement: Scalar import derives an owned ephemeral key

`silk.x25519` SHALL export an owned `X25519` key and `X25519Error`. `fromSecret(bytes)` SHALL borrow exactly 32 little-endian scalar bytes, apply RFC 7748 clamping and return a key with its matching canonical 32-byte public u-coordinate computed with basepoint 9. Other lengths SHALL return `InvalidScalarLength`. `publicKey(self)` SHALL borrow the key and return its public bytes without exposing private scalar storage.

#### Scenario: Derive RFC exchange keys

- **WHEN** Alice's and Bob's RFC 7748 secret byte sequences are imported
- **THEN** their published canonical public keys are returned

#### Scenario: Reject an invalid scalar width

- **WHEN** scalar input has a length other than 32
- **THEN** import returns `InvalidScalarLength` without constructing a key

### Requirement: Agreement consumes the key and admits RFC coordinates

`agree(self, peer)` SHALL consume the owned key, borrow exactly 32 little-endian peer bytes and return a canonical 32-byte shared result. It SHALL mask the peer high bit and reduce noncanonical coordinates as required by RFC 7748, without an on-curve test. Other peer widths SHALL return `InvalidPeerLength`. An all-zero result SHALL return `AllZeroSharedSecret` without publishing secret bytes. The owner SHALL remain consumed after either success or failure.

#### Scenario: Complete both directions of an exchange

- **WHEN** Alice agrees with Bob's public key and Bob agrees with Alice's public key
- **THEN** both operations return the RFC shared result

#### Scenario: Apply mandated decoding

- **WHEN** the peer high bit is toggled, or p+9 is supplied instead of 9
- **THEN** the result is equivalent to the corresponding canonical input

#### Scenario: Admit a twist input

- **WHEN** a 32-byte twist u-coordinate gives a nonzero X25519 result
- **THEN** agreement succeeds without a P-256-style on-curve restriction

#### Scenario: Reject low-order inputs

- **WHEN** a canonical or noncanonical peer encoding yields an all-zero result
- **THEN** agreement returns `AllZeroSharedSecret` and exposes no result bytes

#### Scenario: Prevent reuse of an owner

- **WHEN** a caller uses a key after passing it to agreement
- **THEN** ordinary ownership analysis rejects the use

### Requirement: Generation obtains explicit fresh entropy

`generate()` SHALL require exclusive `Random`, request exactly 32 fresh bytes on each invocation, clamp them and derive the corresponding public key. It SHALL preserve fatal provider failures, provide no insecure fallback and retain no entropy provider. It SHALL require no allocator.

#### Scenario: Observe fresh generation requests

- **WHEN** a scripted test provider supplies two scalar draws to two generation calls
- **THEN** exactly two 32-byte requests occur and both returned public keys match their clamped draws

#### Scenario: Require an explicit provider

- **WHEN** a caller reaches generation without supplying Random
- **THEN** the missing requirement is diagnosed rather than selecting a hidden provider

### Requirement: Arithmetic and assurance stay bounded

The implementation SHALL use ordinary-source private bounded arithmetic with fixed-work secret multiplication, no secret-indexed table and no external crypto fallback. Documentation SHALL distinguish functional and generated-code evidence from constant-time, secure-erasure or production-security assurance. Runtime success/rejection SHALL be covered by the shared native corpus and one small LLVM-to-Wasm witness; stress iteration sweeps SHALL remain opt-in.

#### Scenario: Execute portably

- **WHEN** a deterministic known-answer witness is compiled through LLVM for native and wasm32
- **THEN** both produce the expected result without a crypto provider or new intrinsic
