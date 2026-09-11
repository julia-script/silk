## Why

The selected TLS cryptographic profile lacks X25519 public-key derivation and shared-secret agreement. JUL-178 authorizes a portable ordinary-source implementation with explicit fresh entropy and bounded private arithmetic.

## What Changes

- Add `silk.x25519` with an owned ephemeral key, deterministic scalar import, canonical public-key derivation, consuming agreement and explicit Random generation.
- Apply RFC 7748 scalar clamping, coordinate masking and reduction; reject wrong lengths and all-zero results without an on-curve restriction.
- Add pinned known answers, shared native acceptance, ownership/requirement diagnostics, one Wasm witness and generated-code inspection evidence.
- Update manifest, generated library documentation and prescriptive reference together.

## Capabilities

### New Capabilities

- `x25519`: Portable, bounded X25519 ephemeral key agreement and explicit entropy acquisition.

### Modified Capabilities

None. The existing Random service contract is used unchanged.

## Impact

Standard-library source/catalog, generated public documentation, shared acceptance fixtures and reference docs. No compiler privilege, new intrinsic, external crypto dependency or TLS protocol state is introduced.
