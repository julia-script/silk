## Why

HTTP Basic authentication and WebSocket handshakes both need the same standard-padded Base64 byte transformation, but Silk has no reusable public codec. The only existing decoder is private to certificate PEM parsing and deliberately owns incompatible whitespace, framing, limit, and diagnostic policy.

## What Changes

- Add the public `silk.base64` standard-library module with a stateless `Base64` actor.
- Add checked encoded sizing and strict, exact decoded sizing with structured `Base64Error` and `Base64Reason` failures.
- Add caller-buffer `encodeInto` and `decodeInto` operations that allocate nothing, preserve the untouched suffix, and leave the complete destination unchanged on every failure.
- Export the module through the generated standard-library catalog and document the public API, strict validation order, examples, and excluded variants.
- Add bounded cross-target acceptance evidence using independent RFC 4648 vectors and focused malformed-input cases.

## Capabilities

### New Capabilities

- `base64-codec`: Standard-padded Base64 sizing, allocation-free encoding, strict canonical decoding, deterministic diagnostics, and target-portable behavior.

### Modified Capabilities

None.

## Impact

The change adds `packages/compiler/stdlib/silk/base64.silk`, a new standard-library manifest/catalog entry, focused compiler acceptance fixtures, and public standard-library reference documentation. It introduces no compiler intrinsic, provider, allocation requirement, OS import, or network fallback. HTTP client, proxy, and WebSocket policy remain in their owning capabilities and will consume this codec separately.
