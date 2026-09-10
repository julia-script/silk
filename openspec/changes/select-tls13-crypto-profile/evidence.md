# Evidence map

These are implementation inputs, not executed Silk conformance results. The design change has no
new runtime tests. Existing SHA/HMAC/HKDF evidence remains with JUL-161/JUL-162.

## Immutable independent oracle

Use Zig commit `e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa`, verified in the local Zig checkout.
All paths below are relative to `lib/std/crypto/` at that exact commit; use no moving default branch.
[Immutable tree](https://codeberg.org/ziglang/zig/src/commit/e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa/lib/std/crypto).
The table maps component arithmetic as well as composed operations. Zig is a computation oracle,
not the authority for Silk policy or a dependency embedded in the future standard library.

| Selected operation                                                       | Authoritative known-answer material                                                                                                                                                                                                                     | Pinned Zig path                               |
| ------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------- |
| SHA-256 / SHA-384                                                        | [NIST CAVP SHA response vectors](https://csrc.nist.gov/projects/cryptographic-algorithm-validation-program/secure-hashing)                                                                                                                              | `sha2.zig`                                    |
| HMAC-SHA-256 / SHA-384                                                   | [RFC 4231 §4](https://www.rfc-editor.org/rfc/rfc4231.html#section-4)                                                                                                                                                                                    | `hmac.zig`                                    |
| HKDF-SHA-256                                                             | [RFC 5869 Appendix A](https://www.rfc-editor.org/rfc/rfc5869.html#appendix-A)                                                                                                                                                                           | `hkdf.zig`                                    |
| HKDF-SHA-384                                                             | JUL-162's pinned independent fixtures; [NIST ACVP HKDF algorithm specification](https://pages.nist.gov/ACVP/draft-hammett-acvp-kas-kdf-hkdf.html) identifies the algorithm, not an RFC SHA-384 vector                                                   | `hkdf.zig` (independent generated outputs)    |
| AES-128/256 and GCM/GHASH                                                | [NIST CAVP GCM Encrypt/Decrypt response sets](https://csrc.nist.gov/projects/cryptographic-algorithm-validation-program/cavp-testing-block-cipher-modes); [SP 800-38D](https://csrc.nist.gov/pubs/sp/800/38/d/final) defines composition and limits     | `aes_gcm.zig`                                 |
| ChaCha20, Poly1305 and combined AEAD                                     | [RFC 8439 §2.8.2 and Appendix A](https://www.rfc-editor.org/rfc/rfc8439.html#section-2.8.2)                                                                                                                                                             | `chacha20.zig`                                |
| P-256 agreement                                                          | [RFC 5903 §8.1](https://www.rfc-editor.org/rfc/rfc5903.html#section-8.1); [NIST ECCCDH component tests](https://csrc.nist.gov/projects/cryptographic-algorithm-validation-program/component-testing)                                                    | `pcurves/p256.zig`                            |
| X25519 agreement                                                         | [RFC 7748 §§5.2, 6.1](https://www.rfc-editor.org/rfc/rfc7748.html#section-5.2)                                                                                                                                                                          | `25519/x25519.zig`                            |
| ECDSA-P256/SHA-256 verification                                          | [NIST CAVP FIPS 186-4 ECDSA SigVer response sets](https://csrc.nist.gov/projects/cryptographic-algorithm-validation-program/digital-signatures)                                                                                                         | `ecdsa.zig`, `pcurves/p256.zig`               |
| RSA-PSS/SHA-256 (including MGF1) and RSA-PKCS1-v1_5/SHA-256 verification | [NIST CAVP FIPS 186-4 RSA signature verification response sets](https://csrc.nist.gov/projects/cryptographic-algorithm-validation-program/digital-signatures); [RFC 8017 §§8–9](https://www.rfc-editor.org/rfc/rfc8017.html#section-8) defines encoding | `Certificate.zig` (nested RSA implementation) |
| HKDF-Expand-Label / Derive-Secret SHA-256                                | [RFC 8448 §3](https://www.rfc-editor.org/rfc/rfc8448.html#section-3): intermediate secrets, traffic key/IV and finished derivation                                                                                                                      | `tls.zig`, `hkdf.zig`                         |
| HKDF-Expand-Label / Derive-Secret SHA-384                                | Same framing definition, with pinned-oracle-generated SHA-384 inputs/outputs; do not claim RFC 8448 SHA-384 vectors                                                                                                                                     | `tls.zig`, `hkdf.zig`                         |

## Fixture selection contract for follow-ups

Each implementation must commit selected inputs and expected outputs, source URL/section or exact
archive member and case identifier, downloaded corpus checksum, independent-oracle commit, and the
reproduction command/tool version. NIST landing pages were checked for available vector families;
individual ZIP members are not pinned by this design. Pin the actual downloaded subset before
implementation handoff. This is mechanical fixture selection within the fixed algorithm contract,
not permission to change the profile. No live network dependency belongs in the default tests.

Use authoritative vectors for AES key sizes, encryption/decryption and empty/nonempty AAD;
ChaCha/Poly1305 component plus AEAD composition; both ECDHE groups; successful and invalid
ECDSA/RSA signatures; and TLS intermediate outputs. Supplement with a small, pinned adversarial
subset (for example [Wycheproof](https://github.com/C2SP/wycheproof)) after checking its licensing
and revision. Do not introduce exhaustive external corpora into default CI.

Distinct negatives include tag/AAD/nonce/ciphertext tampering and destination preservation,
P-256 invalid points, X25519 all-zero result and specified noncanonical decoding, ECDSA DER/range
errors (and valid high-s), RSA representative/length/padding/PSS-parameter errors and each selected
policy restriction. Boundary rejection should use structural analysis or small primitive calls,
not giant allocations, expensive ordinal sweeps or per-feature native compiler harnesses.

For labels, verify exact encoded info and independently derived bytes for both hashes, raw-label
and context maxima and one excess, invalid label with zero output, maximum output admission and
one excess rejection, and messages/from-hash equivalence. Do not rerun the complete generic HKDF
corpus. RFC 8448 contains historical small RSA test keys: its key-schedule inputs are useful without
admitting those keys through the production RSA policy.

## Honest assurance

Passing CAVP vectors is not CAVP/FIPS certification. An independent implementation can share defects;
retain primary known answers and negative-policy cases. Functional vectors do not establish timing
or physical side-channel resistance. Public-target output review and documented limitations remain
separate acceptance evidence. Entropy's current fatal-failure behavior is an existing Silk contract,
not something the independent oracle is allowed to replace with a recoverable fallback.
