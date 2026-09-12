## Why

JUL-179 requires deterministic verification of ECDSA P-256/SHA-256 messages and retained certificate signature metadata. The admitted base b7d31aa1f34bd971cb55f5b60e629615ed976c61 supplies JUL-177's private curve arithmetic but no signature verification. Delivery remains dependent on JUL-177's review and merge.

## What Changes

- Add a borrowed-message EcdsaP256Sha256 verifier in the owning p256 module, hashing once and rejecting lengths outside SHA-256's byte domain.
- Admit only canonical uncompressed P-256 keys and strict DER signatures, accepting both high and low s and correctly handling zero reduced digests and identity results.
- Add a metadata adapter for existing borrowed AlgorithmView and BitStringView values with exact P-256 and ecdsa-with-SHA256 parameter policy.
- Reuse private bounded arithmetic without adding a public prehashed API, bigint facility, signing, entropy requirement, or certificate trust policy.
- Add pinned independent fixtures, consolidated native acceptance, a small Wasm witness, public docs and generated artifacts.

## Capabilities

### New Capabilities

- `ecdsa-p256-verification`: bounded ECDSA P-256/SHA-256 message and certificate-metadata verification.

### Modified Capabilities

None. Existing P256 agreement semantics remain unchanged.

## Impact

Touches the p256 standard-library owner, its manifest aliases and generated embedding/documentation, shared compiler acceptance fixtures, and this OpenSpec change. Uses existing SHA-256 and certificate metadata actors; adds no compiler privilege, host dependency, allocator, or runtime provider.
