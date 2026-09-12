## Why

A trusted certificate chain does not establish that its leaf identifies the intended HTTPS
origin. Silk needs one explicit name-matching contract before certificate, trust and TLS work
can compose without accepting names supplied by DNS resolution or the peer.

## What Changes

- Implement caller-owned origin inputs, borrowed SAN descriptors, DNS-ID/IP-ID matching,
  typed failures, deterministic malformed-entry handling and resource limits in `silk.https_identity`.
- Implement bounded certificate GeneralNames decoding into caller-owned descriptor storage in
  `silk.certificate_identities`.
- Execute the version-pinned adversarial fixture matrix and preserve the documented RFC/webpki differences.
- Publish public source documentation and generated stdlib registration/reference pages.

The original JUL-169 design handoff is complete. Julia subsequently authorized implementation of
all created specifications; this change now includes runtime follow-through for JUL-183 and JUL-184.

## Capabilities

### New Capabilities

- `https-service-identity`: HTTPS reference selection, bounded DNS-ID/IP-ID matching and SAN adaptation.

### Modified Capabilities

None. Certificate envelope decoding remains unchanged; the identity consumer decodes SAN content.

## Impact

Ordinary Silk modules, manifest/generated stdlib sources, documentation and focused acceptance fixtures.
No compiler privilege, runtime provider or external dependency is added. The pure matcher remains
usable without a certificate decoder. The SAN adapter consumes the existing certificate extension API.
Generic URI syntax, textual IP conversion, trust/path policy, name constraints and TLS composition
remain separately owned integration work.
