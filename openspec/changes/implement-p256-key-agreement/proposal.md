## Why

JUL-177 supplies the selected P-256 key agreement group. SHA and HKDF cannot produce a peer key share or an ECDHE secret, and JUL-179 needs the same owning curve arithmetic for verification.

## What Changes

- Add ordinary-source `silk.p256` with scalar admission, uncompressed public-key derivation, validated ECDH, and explicit Random generation.
- Keep bounded arithmetic with the P-256 owner. No general bigint API, crypto intrinsic, external provider, or TLS state machine.
- Publish typed admission errors, reference documentation, pinned vectors, shared native cases, and an LLVM-to-Wasm witness.

## Capabilities

### New Capabilities

- `p256-key-agreement`: P-256 scalar/point admission, ephemeral generation and shared-secret derivation.

### Modified Capabilities

None.

## Impact

Compiler stdlib source/manifest/embedding, generated API documentation, prescriptive runtime reference, and native acceptance fixtures. Existing Random retains its fatal provider-failure policy. No compiler semantics change.
