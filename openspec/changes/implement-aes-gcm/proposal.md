## Why

JUL-175 supplies the authenticated cipher missing from the selected TLS profile. Existing hashes and HKDF cannot protect record payloads or authenticate modified ciphertext.

## What Changes

- Add ordinary-source `silk/aes_gcm` with detached AES-128/256-GCM seal/open, caller-owned destinations and typed rejection before mutation.
- Keep table-free AES and fixed-schedule GHASH private, with bounded storage and no external crypto dependency.
- Add independent fixtures, economical runtime and ownership evidence, public documentation and target-output review.

## Capabilities

### New Capabilities

- `aes-gcm`: Bounded detached AES-128/256-GCM authenticated encryption.

### Modified Capabilities

None.

## Impact

Standard-library source/manifest/generated documentation, the shared native acceptance corpus, a small LLVM-to-Wasm witness and the prescriptive runtime reference. No TLS records, nonce allocation, platform provider, compiler privilege or general bigint API is introduced.
