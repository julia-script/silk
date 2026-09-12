## Why

JUL-176 supplies the authenticated cipher missing from the selected TLS_CHACHA20_POLY1305_SHA256 profile. Hashing and key derivation cannot encrypt a payload or verify its Poly1305 tag.

## What Changes

- Add ordinary-source IETF ChaCha20-Poly1305 seal/open with borrowed inputs, caller-owned output and detached tags.
- Authenticate before plaintext writes and reject every invalid call without changing any destination.
- Keep ChaCha20 and Poly1305 arithmetic private, bounded, and independent of platform crypto and compiler special cases.
- Add authoritative component/composition fixtures, native acceptance, a small LLVM-to-Wasm witness, and generated documentation.

## Capabilities

### New Capabilities

- `chacha20-poly1305`: Bounded IETF authenticated encryption and explicit failure/nonce contracts.

### Modified Capabilities

None.

## Impact

Standard-library source and manifest, generated compiler source and API docs, runtime reference, and the shared native acceptance corpus. No networking, TLS state machine, entropy provider, public bigint framework, or compiler intrinsic is introduced.
