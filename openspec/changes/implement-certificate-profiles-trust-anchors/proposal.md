## Why

Silk's structural certificate decoder deliberately preserves security-relevant material without deciding whether it is acceptable for TLS. Callers now need one bounded, portable semantic layer that admits the project’s fixed TLS-server certificate profile, verifies issuer signatures, and represents explicitly trusted certificates as independent owned values without accidentally treating parsing or storage as authentication.

## What Changes

- Add `silk/certificate_profile` with allocation-free role inspection, strict extension/key/signature-metadata policy, borrowed semantic views, owned diagnostics, and issuer-linkage/signature verification.
- Add `silk/trust_anchor` with opaque ownership-moving constructors, independently owned cloning, optional configured path-length and name-constraint restrictions, precise retained-byte accounting, and no implicit validation or OS policy.
- Expose the existing strict P-256 public-point admission as a small reusable operation and admit both absent and NULL `rsaEncryption` parameters where the fixed certificate profile permits them.
- Register the actors, generate reference documentation, and add economical native plus LLVM-to-Wasm evidence with pinned fixture provenance.
- Keep path construction, time/path/identity validation, trust-source acquisition, revocation, and network policy out of this change.

## Capabilities

### New Capabilities

- `certificate-profiles-trust-anchors`: Bounded TLS-server certificate semantic views, exact issuer-signature checks, and owned constrained trust-anchor values.

### Modified Capabilities

None.

## Impact

This adds ordinary Silk source under `packages/compiler/stdlib/silk`, entries in the canonical standard-library manifest, compiler acceptance fixtures/corpus registrations, generated API/reference output, and focused documentation. It depends only on the delivered certificate decoder, SAN adapter, P-256/ECDSA, and RSA actors. JUL-168 and JUL-170 may consume the resulting public APIs; neither path construction nor trust-source behavior is implemented here.
