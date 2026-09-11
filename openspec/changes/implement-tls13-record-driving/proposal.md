## Why

Silk has the admitted TLS 1.3 AEAD and HKDF primitives but no bounded record layer that can safely retain partial transport input and output. JUL-171 supplies that ownership boundary so the later authenticated client can drive arbitrary byte fragmentation without replaying ciphertext, releasing unauthenticated plaintext, or allocating while processing records.

## What Changes

- Add ordinary-source protected send and receive record actors for the three admitted TLS 1.3 cipher suites, plus separate bounded plaintext-handshake framing.
- Add explicit partial-byte input progress, one-record receive ownership, pending-output acknowledgment, strict record validation, and terminal peer-error behavior.
- Enforce fixed wire/plaintext buffers, record and key-usage limits, atomic caller-error behavior, and authenticated-header/nonces defined by TLS 1.3.
- Integrate the public module into the standard-library manifest and generated reference, with economical native, structured-analysis, and LLVM-to-Wasm evidence plus independently pinned record vectors.
- Explicitly leave handshake state, server authentication, key transitions, transport resources, and connection adaptation to JUL-187/JUL-188.

## Capabilities

### New Capabilities

- `tls13-record-driving`: Bounded TLS 1.3 plaintext/protected record framing and partial-byte transport ownership.

### Modified Capabilities

None.

## Impact

Adds `silk/tls_record` and its public manifest/reference surface. It consumes the delivered TLS HKDF and AEAD actors without compiler recognition, platform crypto, heap growth during record processing, network I/O, or a claim of authenticated HTTPS.
