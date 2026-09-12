## Why

Silk has portable SHA-256 and SHA-384 but lacks the HMAC and HKDF composition needed for protocol key derivation. JUL-162 adds this independently usable foundation for future TLS work.

## What Changes

- Add ordinary-source `HmacSha256` and `HmacSha384` with borrowed input, incremental updates, consuming finalization, and fixed-size authentication tags.
- Add `HkdfSha256` and `HkdfSha384` with fixed-size extract results and bounded expansion into caller-owned output.
- Expose a typed output-length error that leaves rejected output unchanged.
- Publish canonical modules, generated surfaces, documentation, and consolidated known-answer acceptance evidence.

## Capabilities

### New Capabilities

- `bootstrap-hmac-hkdf`: Concrete portable HMAC and HKDF over SHA-256 and SHA-384.

### Modified Capabilities

None. Existing SHA lifecycle requirements remain unchanged.

## Impact

The compiler-shipped standard library gains `silk/hmac` and `silk/hkdf`. Manifest-derived compiler and documentation surfaces and the native acceptance corpus change. No compiler privilege, dependencies, providers, or target-specific implementation are introduced.

## Non-goals

Generic hash abstraction, tag verification, constant-time comparison, truncation policy, TLS labels or handshakes, certificates, other cryptographic primitives, password hashing, and zeroization guarantees.
