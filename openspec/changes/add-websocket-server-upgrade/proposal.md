## Why

The scoped HTTP server can transfer a buffered channel, but applications still lack a validated WebSocket handshake. JUL-202 supplies one request-bound authorization boundary so malformed requests cannot switch protocols or lose read-ahead bytes.

## What Changes

- Add ordinary-source `silk.websocket_upgrade` inspection, bounded negotiation, typed errors, and scoped handoff.
- Reuse strict Base64, SHA-1, HTTP values, and the existing server resource scope.
- Add offline protocol and scoped transport evidence, generated registration, and public documentation.

## Capabilities

### New Capabilities

- `websocket-server-upgrade`: strict server handshake validation, explicit application policy, bounded response planning, and exclusive buffered handoff.

### Modified Capabilities

None.

## Impact

Standard library, compiler acceptance fixtures, generated catalogs, and public reference. No compiler intrinsic, frame codec, compression, or TLS server is added. Prerequisites are present at `e253eaa219856d3bc5809845cac5562d88cf7cf7`.
