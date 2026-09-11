## 1. Public semantic actors

- [x] 1.1 Add the certificate-profile public data/error/limit/view types and immutable getters; verify the manifest/API extractor resolves every declared import and exported member.
- [x] 1.2 Implement deterministic version, serial, unique-ID, key, signature-metadata, and extension-order admission; verify focused semantic cases cover supported roles and each distinct rejection class.
- [x] 1.3 Implement strict bounded BC, KU, EKU, SAN, NC, policy, AKI/SKI, and unknown-extension handling; verify zero/inclusive limits, malformed values, duplicate/critical cases, and retained borrowed DER views.
- [x] 1.4 Implement exact issuer linkage and ECDSA/RSA signature verification using original retained bytes; verify valid, invalid, mismatched, absent-vs-NULL, and PSS-default cases.

## 2. Primitive and ownership support

- [x] 2.1 Expose the existing P-256 strict point admission without duplicating arithmetic and update RSA certificate-key parameter admission; verify existing primitive acceptance sources plus profile integration remain green.
- [x] 2.2 Add byte/offset-preserving independent `Certificate.copy`; verify the clone has identical DER/views and survives release of the original owner.
- [x] 2.3 Add opaque `TrustAnchor` ownership-moving constructors, clone, getters, and checked encoded-byte accounting; verify unsupported storage, configured/embedded provenance, allocation failure cleanup, and no partial owner.

## 3. Portable evidence and fixture provenance

- [x] 3.1 Add one table-driven native acceptance source and cheap compiler ownership assertions for all distinct JUL-185 claims; verify the focused native corpus case and ownership tests pass.
- [x] 3.2 Add one compact LLVM-to-Wasm witness using the same public actors; verify the focused driver test completes without target-specific imports.
- [x] 3.3 Record selected Apache-2.0 x509-limbo fixture IDs, upstream commit/hash, per-fixture hashes, and profile-specific outcomes; verify fixture-integrity tooling accepts the manifest.

## 4. Registration, documentation, and validation

- [x] 4.1 Register the new ordinary-source actors in the canonical standard-library manifest and generate API/reference artifacts; verify focused manifest/catalog/doc checks pass.
- [x] 4.2 Document the restricted profile, explicit trust decision, borrowed inspection boundary, and non-authentication caveats in the public runtime/standard-library reference; verify generated links and formatting.
- [x] 4.3 Run strict OpenSpec validation, focused tests/timing, `pnpm typecheck`, `pnpm format:check`, and `pnpm lint`; record exact results and leave full `pnpm test`, `pnpm check`, and release-candidate verification to exact-head CI per the user override.
- [x] 4.4 Split signature-parameter and empty-subject failures into public semantic reasons; verify exact consolidated algorithm/SAN distinctions, regenerate artifacts, and rerun the focused delivery gates.
