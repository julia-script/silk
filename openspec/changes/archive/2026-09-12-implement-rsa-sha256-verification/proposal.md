## Why

The selected TLS/Web PKI profile needs RSA signature verification, while the delivered certificate
decoder only preserves encoded keys and algorithms. A bounded verifier prevents consumers from
inventing permissive padding or parameter checks.

## What Changes

- Add ordinary-source RSA public-key admission and SHA-256 PSS/PKCS#1 v1.5 verification.
- Interpret the existing decoder's borrowed key and algorithm metadata under the selected profile.
- Bound arithmetic and parsing, reject invalid inputs before exponentiation, and document typed failures.
- Add pinned vectors, shared native rejection evidence, a small Wasm witness, and generated references.

## Capabilities

### New Capabilities

- `bounded-rsa-verification`: RSA public arithmetic, strict signature encodings and algorithm admission.

### Modified Capabilities

None. Certificate containers, generic SHA-256 and standard-library resolution retain their owners.

## Impact

Adds `silk/rsa`, manifest entries and generated documentation. No signing, private-key arithmetic,
public bigint API, entropy, external crypto provider, compiler privilege, TLS state or trust decision.
