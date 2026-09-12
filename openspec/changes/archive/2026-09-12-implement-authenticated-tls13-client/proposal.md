## Why

Silk has bounded TLS record ownership, certificate-path validation, HTTPS identity matching, and explicit trust snapshots, but it cannot combine them into an authenticated TLS 1.3 client. Applications therefore cannot exchange HTTPS plaintext without building security-critical handshake and transcript policy themselves.

## What Changes

- Add an ordinary-Silk transport-independent TLS 1.3 client with bounded input, output, handshake, certificate, ticket, and control-message state.
- Authenticate one server with explicit trust and time before any plaintext becomes visible or writable.
- Support the three selected TLS 1.3 suites, X25519 and P-256 key exchange with one HelloRetryRequest, the selected certificate algorithms, ALPN, compatibility CCS, optional initial client-certificate decline, KeyUpdate, tickets, directional close, truncation, and sticky fatal errors.
- Add non-consuming SHA-256 and SHA-384 transcript checkpoints without duplicating the SHA implementations.
- Add immutable offline replay fixtures captured with pinned rustls, focused native and WebAssembly evidence, generated documentation, and CI selection.

## Capabilities

### New Capabilities

- `authenticated-tls13-client`: The owned client API, authenticated handshake, bounded state machine, traffic, post-handshake controls, closure semantics, fixtures, and public documentation.

### Modified Capabilities

- `bootstrap-cryptographic-hashes`: Non-consuming SHA-256 and SHA-384 checkpoints for exact TLS transcript boundaries.

## Impact

The change adds `silk/tls_client`, extends `silk/sha2`, and updates the standard-library manifest, generated source and reference documentation, focused shared compiler acceptance programs, one WebAssembly witness, deterministic fixture metadata, OpenSpec artifacts, and PR CI smoke selection. It adds no compiler-known TLS actor, ambient trust or time, network transport, verification bypass, resumption, client credentials, TLS 1.2, or production WebAssembly transport.
