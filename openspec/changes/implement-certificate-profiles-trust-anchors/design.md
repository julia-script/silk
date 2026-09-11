## Context

See [proposal.md](proposal.md) for motivation and [the delta specification](specs/certificate-profiles-trust-anchors/spec.md) for the observable contract. The delivered `Certificate` owner preserves exact DER, offsets, algorithms, bits, names, times, unique IDs, and raw extensions; `CertificateSan`, `HttpsIdentity`, `EcdsaP256Sha256`, and `RsaPublicKey` provide the existing semantic and cryptographic building blocks. Ordinary `.silk` source is not covered reliably by the repository graph, so the implementation design is grounded in direct inspection at work base `4ac4ffebd7c0258c4e93ebb5e084660278be88be`.

The certificate decoder intentionally admits material this profile must reject. Conversely, explicit trust-anchor storage must preserve even unsupported material so one unusable root does not poison a whole future trust snapshot. Those boundaries must remain visibly separate.

## Goals / Non-Goals

**Goals:**

- Keep all certificate policy in ordinary allocation-free Silk source except the explicit owned copies needed by anchors.
- Produce deterministic, owned errors carrying enough stable location metadata for callers and tests.
- Preserve exact retained bytes for all signature operations and configured restrictions.
- Make later path construction able to distinguish embedded from configured anchor constraints.

**Non-Goals:**

- Path search, cumulative NameConstraints application, current-time validation, service identity matching, revocation, policy trees, root acquisition, or provider selection.
- Complete RFC 5280, browser Web PKI, CA/B Forum, OS trust-policy, CT, OCSP, CRL, or AIA behavior.
- General-purpose ASN.1, X.509 extension, signature-algorithm, or name-normalization APIs.

## Decisions

### Separate borrowed semantic profiles from owned trust authority

`CertificateProfile<'a>` stores a private `&'a Certificate` plus small copied enum/boolean/optional scalar state and exact extension offsets. `inspect` computes the state once, and only sibling getters expose it. The role getter is `certificateRole`, because `role` is a reserved Silk keyword. This prevents callers from forging a profile and gives issuer verification a certificate already admitted for its selected role.

`TrustAnchor` instead owns a moved `Certificate`, optional configured path length, optional copied configured NameConstraints bytes, and a preflighted retained-byte count. The simple constructor performs no semantic inspection. The constrained constructor validates only external constraint framing/policy before copying. This makes the authority grant explicit while preserving candidate-level rejection for later validation.

Alternative considered: store only subject/SPKI/constraint slices in anchors. Rejected because it loses unsupported/raw certificate material, creates self-referential ownership pressure, and cannot satisfy independent clone semantics.

### One bounded DER walker specialized to profile extensions

The profile actor uses a small reader over each complete extension value with node/depth counters shared across one inspection. It verifies canonical DER identifiers/lengths and only the schemas the profile interprets. Unknown noncritical extension values remain opaque but count toward the per-value budget; duplicate OIDs are detected in certificate order before semantic dispatch.

BasicConstraints, KeyUsage, EKU, policies, AKI/SKI, SAN, and NameConstraints each have a focused decoder. SAN framing and descriptor semantics reuse `CertificateSan`; the profile adds only policy checks needed for criticality and DNS/IP admission. NameConstraints uses the same canonical reader but retains the complete original extension value for later intersection rather than constructing a second owned tree.

Alternative considered: add semantic fields to `Certificate`. Rejected because that would collapse a general structural container into this deliberately restricted TLS-server profile and make unsupported anchors impossible to retain.

### Cache only facts needed by later consumers

The profile exposes `CertificateKeyKind` (`P256` or `Rsa`), role, BC `ca`, optional path length, optional KU bit mask, optional EKU bit mask, and optional original SAN/NC DER. KeyUsage and ExtendedKeyUsage use public Copy structs with named booleans so downstream code does not depend on DER bit positions or OID bytes. Absence remains `Option.None`; present-but-empty or malformed values fail.

AKI/SKI are validated as noncritical hints but not cached because they cannot replace exact issuer-name linkage or signature verification. CertificatePolicies is structurally validated but no accepted policy set is exposed. Prohibited policy/TLS Feature extensions fail by occurrence.

### Make error location and precedence explicit

`ProfileError` owns `ProfileClass`, `ProfileReason`, optional extension index, `ProfileOffsetSpace`, and a byte offset. Certificate offsets are derived by locating borrowed extension/subfield slices inside `Certificate.der`; configured constraints use their own offset space. Failures are selected in the ticket’s fixed order: version/serial/IDs; algorithms/key; extensions in certificate order; then role/usage.

All additions, subtractions, counters, and bounds are checked before iteration or allocation. The constrained anchor constructor validates and checks `certificate.der.length + configured.length` before `Bytes.copy`; ordinary affine cleanup drops the consumed certificate and any partial copy if the Effect fails.

### Reuse cryptographic primitives without a second verifier

P-256 exposes its existing strict point admission as `P256.validatePublicKey`; it delegates to the same private point routine used by ECDSA verification. RSA certificate-key admission is adjusted to accept either absent or NULL `rsaEncryption` parameters while preserving all mathematical bounds.

Inspection normalizes only the equivalence explicitly allowed by the profile: absent-vs-NULL for RSA AlgorithmIdentifiers. It otherwise requires byte-semantic equality of admitted effective signature parameters. `verifyIssuedBy` compares the complete DER names exactly and passes the original TBS and signature bytes directly to `EcdsaP256Sha256.verifyCertificate` or `RsaPublicKey.verifyCertificate`, mapping every primitive rejection to a profile failure.

Alternative considered: re-encode AlgorithmIdentifiers or TBS. Rejected because signature validity is over original bytes and normalization would erase evidence.

### Economical evidence and pinned provenance

One generated acceptance source is registered once in the shared native corpus. It uses existing decoded certificate/primitive material and a compact table of mutations that each distinguish a semantic profile rule, signature/linkage outcome, or ownership property. Compiler ownership assertions cover move/borrow/clone behavior without launching native binaries per case. One deliberately small source subset is reused by the existing LLVM-to-Wasm driver test.

Selected x509-limbo cases are recorded in a fixture manifest with upstream commit `3f8cba420e90322223486086054401189b7b320e`, upstream `limbo.json` SHA-256 `563805f46937ad25ac9d4e41341c414070aced32a22294821b5c5fe526e2c52d`, per-certificate hashes, Apache-2.0 provenance, and profile-specific inspection outcomes. No full corpus sweep is added.

The fixed Zig HTTP parity snapshot is `1bc892110da738d6137b3f0b7e8e3a586ce09928`. It is a comparison provenance point for profile choices only, not a path-building, trust-policy, or authentication oracle.

## Risks / Trade-offs

- [A specialized extension walker can drift from the structural decoder] → Keep it scoped to complete already-decoded extension values, reuse shared SAN behavior, preserve original bytes, and pin malformed/duplicate/critical cases.
- [A fixed profile rejects certificates accepted by broader PKIs] → Return `Unsupported` per candidate and document the exact restricted profile rather than adding fallback behavior.
- [Large acceptance fixtures can inflate the compiler suite] → Use one packed/table-driven corpus program, one compact Wasm witness, cheap ownership analysis assertions, and require measured test-economics approval.
- [Adding a certificate copy operation expands a public actor] → Implement it as byte/offset-preserving ownership support with the same allocator boundary; it performs no parsing or semantic interpretation.
- [Configured constraints could accidentally replace embedded policy] → Store both sources separately and expose only immutable getters; intersection remains the future path validator’s responsibility.

## Migration Plan

Add and register the new actors atomically with their documentation and evidence. There is no existing public profile or trust-anchor API to migrate. A rollback removes the new manifest entries, actors, fixture registrations, and documentation together; no persisted data format is introduced.
