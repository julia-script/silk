## Why

Silk now has SHA-2, HMAC and HKDF, but no common algorithm contract for TLS and certificate consumers. Selecting one bounded profile prevents incompatible primitive work and makes the remaining cost of authenticated HTTPS visible.

## What Changes

- Select a TLS 1.3 server-authenticated client profile with ephemeral key agreement and no older-version fallback.
- Specify separate cipher-suite, key-agreement, handshake-signature and certificate-signature matrices, including the RFC 8446 interoperability floor and recommended ciphers/groups.
- Define TLS label framing above JUL-162's delivered generic HKDF, explicit target/entropy boundaries, parameter rejection and security limitations.
- Pin conformance evidence and split missing primitive implementation into independently estimated Linear follow-ups.

These are intended contracts. This design-only change ships no cryptographic or TLS implementation and does not change the published standard-library reference or manifest.

## Capabilities

### New Capabilities

- `tls13-crypto-profile`: Algorithm selection, typed primitive boundaries, provider/target policy and verification evidence for the first TLS/Web PKI profile.
- `tls13-key-derivation`: Bounded HKDF-Expand-Label and Derive-Secret composition over existing SHA-256/SHA-384 HKDF.

### Modified Capabilities

None. Existing cryptographic-hash, random and HMAC/HKDF contracts remain owned by their original changes.

## Impact

Planning artifacts and Linear decomposition only. Future implementations will add ordinary Silk actors and their manifest/reference/test integration. This profile feeds [JUL-171](https://linear.app/juliaortiz/issue/JUL-171) and [JUL-168](https://linear.app/juliaortiz/issue/JUL-168); generic HMAC/HKDF remains [JUL-162](https://linear.app/juliaortiz/issue/JUL-162).

## Non-goals

Record or handshake implementation, certificate decoding, path construction/validation, identity matching, trust discovery, sockets, client signing, TLS servers, resumption/PSK/0-RTT, QUIC/DTLS, general-purpose big integers, platform crypto wrappers, and claims of audited constant-time code or secure erasure.
