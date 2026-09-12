## Why

JUL-181 implements the accepted JUL-166 TLS label contract over delivered generic HKDF. Raw HKDF info cannot enforce TLS domain separation or length boundaries.

## What Changes

- Add ordinary-source TLS SHA-256 and SHA-384 label and secret-derivation actors.
- Validate public lengths before output writes or hashing, and keep transcript messages distinct from fixed-width hashes.
- Integrate manifest, generated references, independent vectors, native acceptance and a small LLVM-to-Wasm witness.

## Capabilities

### New Capabilities

- `tls13-key-derivation`: Deliver the selected bounded TLS label operations.

### Modified Capabilities

None.

## Impact

Adds `silk/tls_hkdf`; leaves generic HKDF ownership and behavior unchanged. No TLS protocol or complete-profile support claim.
