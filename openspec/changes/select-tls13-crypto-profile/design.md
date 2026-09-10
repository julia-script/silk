## Context

See [proposal.md](proposal.md) for scope. At clean work base
`c6eae2f976a8f1a7681ffbfdcbbc8c776b9f0c96`, the manifest and source contain SHA-2,
HMAC-SHA-256/SHA-384 and HKDF-SHA-256/SHA-384. JUL-162 is Done; its active
`add-hmac-hkdf-foundations` artifacts record delivered implementation. It owns the generic
operations permanently. The change from the triage base
`f5ada543a1738e4f0c69b311bdeb9a72e32e9dd3` also delivers compression, URI and canonical imports.
None supplies TLS labels or the missing encryption, key agreement and signature actors.

Graph discovery was followed by direct source/manifest checks because `.silk` paths have no
tracked graph coverage. Relevant source: `packages/compiler/stdlib/silk/{sha2,hmac,hkdf,random,os_random}.silk`;
relevant evidence: `packages/compiler/test/support/hmacHkdfAcceptance.ts` and
`apps/docs/content/reference/runtime-and-standard-library.md`. The existing random contract is
`openspec/specs/bootstrap-random/spec.md`. This design does not modify those contracts.

## Goals / Non-Goals

The output is a closed initial algorithm selection and a separable implementation backlog.
“Required” below means required to claim this profile in a future release, not currently shipped.
“Deferred” means unavailable in this profile, not an implicit platform-dependent option.
The cryptographic boundary verifies bytes and parameters; it does not decide whether a peer,
certificate path or identity is trusted. Those decisions retain their separate issue owners.

## Decisions

### Protocol and algorithm matrix

Select a TLS 1.3-only client over a reliable byte stream, with server certificate authentication
and fresh ephemeral ECDHE. Do not offer PSK, resumption, 0-RTT, client authentication, TLS 1.2,
DTLS or QUIC. A server requiring an excluded mode cannot be used. Record/handshake state machines,
including retries and key updates, belong to JUL-171.

| Surface                      | Required in the selected profile                                                                              | Deferred / unavailable                                                            |
| ---------------------------- | ------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------- |
| AEAD + transcript/HKDF hash  | `TLS_AES_128_GCM_SHA256` (0x1301); `TLS_AES_256_GCM_SHA384` (0x1302); `TLS_CHACHA20_POLY1305_SHA256` (0x1303) | CCM and all other suites                                                          |
| Key agreement                | `secp256r1` (0x0017); `x25519` (0x001d)                                                                       | P-384/P-521, X448, finite-field and hybrid/PQ groups                              |
| Server CertificateVerify     | `ecdsa_secp256r1_sha256` (0x0403); `rsa_pss_rsae_sha256` (0x0804)                                             | PKCS#1 v1.5, SHA-1, other hashes/curves, EdDSA, RSA-PSS-key schemes               |
| Certificate-chain signatures | ECDSA P-256/SHA-256; RSA-PSS with rsaEncryption issuer key and SHA-256; RSA-PKCS1-v1_5/SHA-256                | SHA-1/MD5, SHA-384/SHA-512 signatures, P-384/P-521, EdDSA, RSA-PSS issuer-key OID |

The algorithm floor follows [RFC 8446 §9.1](https://www.rfc-editor.org/rfc/rfc8446.html#section-9.1).
Its recommended AES-256, ChaCha and X25519 are also selected as required for this profile.
AES-128-only was considered but rejected: a single explicit portable implementation set avoids
conditional negotiation policy. No optional runtime algorithm is selected in version one.
Suite hashes never select the certificate signature hash or key type.

The narrow certificate matrix is a deliberate interoperability restriction: a valid Web PKI chain
using an excluded signature fails this profile even if its leaf could perform an admitted handshake.
Advertising separate handshake and certificate lists avoids implying that PKCS#1 is a valid
CertificateVerify scheme. Trust-anchor self-signature handling belongs to JUL-168; this primitive
profile does not require a validator to verify a trust anchor's self-signature.

### Key and parameter policy

These are selected policy limits, not a claim that the underlying standards require all limits.
AEAD uses detached tags: ciphertext has the plaintext length and the 16-byte tag is a separate
output/input. AES-GCM plaintext/ciphertext is limited to `2^36 − 32` bytes and AAD to
`2^61 − 1` bytes. ChaCha20-Poly1305 plaintext/ciphertext is limited to
`64 × (2^32 − 1)` bytes and AAD to `2^64 − 1` bytes. Addressable slice and destination
capacity impose additional limits. Check these bounds without overflowing the target usize;
no tag byte contributes to the ciphertext length used by these limits.

| Operation          | Admitted inputs and result                                                                                                                                               | Rejection boundary                                                                                          |
| ------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------- |
| AES-GCM            | 16/32-byte key; 12-byte nonce; 16-byte tag; caller-owned ciphertext/plaintext and borrowed AAD                                                                           | Wrong lengths, insufficient output, arithmetic/algorithm length overflow; wrong tag is AuthenticationFailed |
| ChaCha20-Poly1305  | IETF 32-byte key, 12-byte nonce, 16-byte tag; caller-owned buffers                                                                                                       | Same boundary; reject counter exhaustion before writing                                                     |
| P-256 ECDHE        | Private scalar 1..n−1; peer SEC1 uncompressed 65-byte point; fixed 32-byte big-endian shared x-coordinate                                                                | Wrong size/prefix, noncanonical/out-of-field coordinates, off-curve or infinity, invalid scalar             |
| X25519             | 32-byte scalar input with RFC 7748 clamping; 32-byte u-coordinate input with mandated masking/reduction; 32-byte result                                                  | Wrong lengths and all-zero shared result; do not invent canonical-input restrictions contrary to RFC 7748   |
| ECDSA verification | P-256 validated public point; SHA-256 digest of borrowed message; strict DER pair of positive r,s in 1..n−1                                                              | Invalid DER/point/range or incorrect signature; accept both high-s and low-s valid signatures               |
| RSA verification   | Positive odd 2048..4096-bit modulus (inclusive, no multiple-of-8 restriction), exponent exactly 65537; rsaEncryption issuer key; signature exactly ceil(modBits/8) bytes | Oversize/undersize, invalid modulus/exponent, representative ≥ n, bad encoding or signature                 |
| RSA-PSS            | SHA-256, MGF1-SHA-256, salt exactly 32 bytes, trailer field 1                                                                                                            | Any parameter mismatch; enforce emBits = modBits−1 and leading-bit constraints                              |
| RSA-PKCS1-v1_5     | SHA-256 DigestInfo and strict complete padding/DER verification, certificate use only                                                                                    | No BER, trailing data, shortened padding, digest-prefix or permissive parsing acceptance                    |

RSA key size/exponent restrictions bound memory and public exponent work; arbitrary big-integer APIs
and RSA private operations would expand the scope without benefiting this client. Encoding policy
follows [RFC 8017](https://www.rfc-editor.org/rfc/rfc8017.html),
[RFC 4055](https://www.rfc-editor.org/rfc/rfc4055.html) and
[RFC 5480](https://www.rfc-editor.org/rfc/rfc5480.html). The certificate decoder (JUL-167) owns
ASN.1 extraction; it must retain enough information to distinguish absent/default/explicit
parameters. The verifier checks semantic values, including RSA-PSS defaults (which imply SHA-1
and are rejected here); RSA encryption and SHA-256-with-RSA identifiers accept absent or NULL
parameters where the applicable certificate RFC permits. ECDSA-with-SHA256 parameters must be
absent; named-curve P-256 is required, never explicit curve parameters. Reject unsupported key
algorithms even when the signature bytes happen to verify.

Expose semantic typed failures for UnsupportedAlgorithm, InvalidKey, InvalidParameters,
InvalidEncoding, InvalidLength, OutputTooSmall, LimitExceeded and AuthenticationFailed.
Concrete Silk error actor spelling is owned by each implementation issue. Rejections must not trap,
expose secret values or partially publish successful results. AEAD open authenticates before
publishing plaintext and leaves output unchanged on failure. Separate input/output borrowing avoids
hidden in-place alias rules. Constant-work tag comparison belongs inside each authenticated
operation, not a generic public comparison service. Nonce uniqueness and per-key usage accounting
remain caller obligations; JUL-171 must impose tighter TLS record/key-use limits. No random nonce
is generated inside AEAD.

### Portable implementation and availability

Choose ordinary source actors for AES/GHASH, ChaCha/Poly1305, elliptic-curve arithmetic, bounded RSA
public verification and TLS label composition. Private arithmetic helpers belong to their owning
concept; do not add a public big-integer framework. There is no external crypto provider in the
initial selection. A platform crypto alternative would need an explicit separately reviewed service,
resource/error contract and target selection; it is not a hidden fallback or a way to call missing
algorithms available. No compiler-known library names or crypto intrinsics are proposed.

| Target/profile                                           | Current SHA/HMAC/HKDF                                                         | Current production entropy              | New primitives/labels now | Intended admission after follow-ups                                                                         |
| -------------------------------------------------------- | ----------------------------------------------------------------------------- | --------------------------------------- | ------------------------- | ----------------------------------------------------------------------------------------------------------- |
| Darwin ARM64, Apple ABI, system libc                     | Ordinary source present                                                       | OsRandom via arc4random_buf             | Absent                    | Portable primitives plus explicit Random provision                                                          |
| GNU/Linux x86-64 or ARM64, GNU ABI/libc                  | Ordinary source present                                                       | OsRandom via nonblocking getrandom      | Absent                    | Same portable primitives plus explicit Random provision                                                     |
| LLVM-to-Wasm wasm32                                      | Ordinary source available to compilation; this design adds no execution proof | No official provider                    | Absent                    | Deterministic primitive KATs; production ECDHE/TLS only with separately admitted host entropy and transport |
| Other native profiles, including raw/no-libc and Windows | Source portability is not a support claim                                     | No official provider in reviewed source | Absent                    | No complete profile claim without explicit target validation and providers                                  |

All current end-to-end TLS cells are **unavailable**. Native code generation and deterministic
WebAssembly execution do not supply sockets, trust, identity or cryptographic assurance. The
retired runtime evaluator and independent direct-Wasm backend are excluded. Future cross-target
proof uses LLVM-generated native and LLVM-to-Wasm only.

Ephemeral-key generation requires the existing exclusive `Random` service. P-256 uses unbiased
rejection sampling, X25519 uses 32 fresh bytes then clamping; never reuse secret ephemeral keys.
Scripted entropy is test-only. Current Random provider failure is fatal, not a typed recoverable
entropy error: preserve that contract and never downgrade to InsecureRandom. Native capability
absence is diagnosed during source selection. Do not promise recoverable entropy failure here.

### TLS labels as a separate ordinary-source boundary

Add proposed `silk/tls_hkdf` concrete SHA-256 and SHA-384 actors. Each borrows a fixed 32/48-byte
secret and input slices, then writes caller-owned output through generic HKDF. This module owns
only framing and derivation, not transcript lifecycle or the TLS state machine.

Semantic operation shapes (notation, not shipped Silk signatures):

```text
expandLabel(secret[HashLen], labelBytes, contextBytes, output) -> Result<(), LabelError>
deriveSecret(secret[HashLen], labelBytes, handshakeMessages) -> Result<bytes[HashLen], LabelError>
deriveSecretFromHash(secret[HashLen], labelBytes, transcriptHash[HashLen]) -> Result<bytes[HashLen], LabelError>

info = u16be(output.length)
     || u8(6 + labelBytes.length) || ASCII("tls13 ") || labelBytes
     || u8(contextBytes.length) || contextBytes

deriveSecret(s, l, messages) = expandLabel(s, l, Hash(messages), HashLen)
```

The convenience operation hashes exactly the supplied concatenated handshake bytes. The
from-hash operation prevents rehashing a caller-owned streaming transcript; fixed-width hash types
separate SHA-256 from SHA-384. JUL-171 owns handshake serialization, message inclusion and
HelloRetryRequest transcript rewriting. Empty messages mean Hash(empty), unlike the zero-byte
context used for traffic key, IV and finished-key expansion.

Raw labels admit 1..249 bytes and contexts 0..255 bytes; no NUL termination, Unicode normalization,
automatic label-prefix detection or trimming. The prefix is added exactly once by the operation,
even when raw input itself contains those bytes. Output admits zero through 8160 (SHA-256) or
12240 (SHA-384); this is also within the encoded u16 limit. Validate lengths before any narrowing,
output write or zero-output fast path. Error kinds LabelLength, ContextLength and OutputLength
carry requested/maximum public lengths; SHA-256 message input beyond its byte-aligned hash domain
must return MessageTooLong before hashing rather than exposing the generic SHA trap. SHA-384's
hash domain exceeds addressable slice lengths on selected targets. Fixed storage for encoded info
is at most 514 bytes; no input-sized heap copy or retained transcript.

The following framing example is deliberately independent of any secret:

```text
SHA-256 expandLabel(label="key", context=[], output.length=16)
info = 00 10 09 74 6c 73 31 33 20 6b 65 79 00
```

This composition is specified by [RFC 8446 §7.1](https://www.rfc-editor.org/rfc/rfc8446.html#section-7.1)
and rests on [RFC 5869](https://www.rfc-editor.org/rfc/rfc5869.html). A generic HKDF info parameter
alone is insufficient to enforce its lengths/domain separation. Arbitrary strings are not a
protocol-label registry: selecting the right labels remains the TLS consumer's responsibility.

### Security and evidence boundary

Ordinary source is selected for portability and inspectability, not as a proof of constant time.
Secret scalar, key, tag and AEAD processing must avoid secret-dependent branches, addresses,
lookup tables and variable-work arithmetic. AES uses a table-free approach; GHASH and curve
multiplication use fixed schedules. RSA verification handles public data but still has bounded
resource use. P-256 scalar generation rejection is probabilistic and must not be confused with a
fixed-work scalar multiplication guarantee.

Functional vectors cannot establish side-channel resistance. Each primitive issue must document
secret-dependent operations and inspect generated code for the supported native optimization modes
and Wasm output; any newly required target-neutral compiler primitive is a separate explicit change.
Unreviewed compiler transformations, host/JIT behavior, physical/power channels and formal proofs
remain outside the assurance claim. Do not advertise production security merely because vectors
pass. No FIPS validation, secure heap, guaranteed zeroization, swap/core-dump exclusion or erasure
of compiler-generated secret copies is promised. Owned lifetimes bound use, not physical retention.

Authoritative vectors, immutable reference paths and issue decomposition are in
[evidence.md](evidence.md) and [implementation.md](implementation.md). Runtime behavior uses the
shared native corpus; structured analysis proves diagnostics, ownership and target absence. Use a
small LLVM-to-Wasm witness for intended portability, not a second exhaustive corpus. Stress sweeps
and timing studies stay opt-in. This planning change adds no permanent executable tests.

## Risks / Trade-offs

- Narrow certificate algorithms, RSA exponent/key bounds and excluded modes reject otherwise valid
  servers → explicit policy failures and documented limits; no silent weakening or alternate provider.
- Secret arithmetic is high-risk and expensive → independent KATs, negative verification cases,
  bounded arithmetic modules and dedicated implementation/security review before support claims.
- AES software can be slow without hardware instructions → start with table-free correctness and
  measure in opt-in benchmarks; optimize only through explicit ordinary-source/target-neutral work.
- A valid signature is not a valid certificate path → keep JUL-167/168/169/170/171 boundaries intact.
- Incomplete primitive delivery could look like an enabled TLS suite → only advertise a complete
  admitted profile after all selected mandatory pieces and protocol work are validated.

## Migration Plan

Merge these planning artifacts, then implement the linked issues separately. Update each actor's
manifest, reference, generated surfaces and tests in its own complete green-field change. Keep the
unchecked implementation tasks visible; do not archive these future behavior deltas as shipped
support on completion of JUL-166. Reverting this design changes no runtime or wire behavior.
