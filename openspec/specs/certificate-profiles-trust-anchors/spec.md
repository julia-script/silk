# certificate-profiles-trust-anchors Specification

## Purpose

Define Silk’s bounded, portable TLS-server certificate semantic profile, exact issuer-signature operation, and explicit owned trust-anchor representation without conflating inspection or storage with authentication.

## Requirements

### Requirement: Semantic inspection exposes immutable role-specific views

The standard library SHALL expose `silk/certificate_profile` with `CertificateProfile<'a>`, `CertificateRole`, `ProfileLimits`, and owned `ProfileError`. `inspect` SHALL allocate nothing, retain a borrow of the complete decoded certificate, and expose the admitted role through `certificateRole`, plus key kind, BasicConstraints/pathLen, KeyUsage, ExtendedKeyUsage, SAN DER, and NameConstraints DER while preserving absence. The getter uses `certificateRole` because `role` is a reserved Silk keyword. Callers MUST NOT be able to forge or mutate cached views. Inspection SHALL not check time, cumulative constraints, trust, identity, or an actual signature.

#### Scenario: Supported server leaf

- **WHEN** a decoded v3 certificate has an admitted key, signature metadata, extensions, and server-leaf usage
- **THEN** inspection succeeds with `ServerLeaf`, returns the original certificate borrow, preserves absent optional values, and exposes any supported extension values

#### Scenario: Unsupported anchor remains storable

- **WHEN** a decoded anchor uses a key or embedded semantic form outside the admitted profile
- **THEN** semantic inspection returns an owned `Unsupported` profile error without altering or consuming the certificate

### Requirement: Certificate admission is strict, bounded, and deterministic

Path certificates SHALL be v3 with positive nonzero serials of at most 20 content octets, zero unused public-key/signature bits, no issuer/subject unique identifiers, and admitted P-256 or RSA public keys. Anchors SHALL admit v1 or v3 and ignore their serial, validity, self-signature, and signature-algorithm metadata while still applying key and embedded-extension policy. Inspection SHALL reject duplicate extension OIDs, unknown critical extensions, malformed recognized values, prohibited policy-processing extensions, TLS Feature, and role-invalid BC/KU/EKU/SAN/NC semantics. `SignatureAlgorithm` SHALL identify unrecognized signature OIDs or invalid signature BIT STRING metadata, `SignatureParameters` SHALL identify forbidden, missing, or unsupported parameters on a recognized admitted OID, and `SignatureAlgorithmMismatch` SHALL identify differing individually admitted inner and outer algorithms. Limits SHALL be inclusive, zero SHALL forbid the corresponding resource, aggregate counters SHALL use checked arithmetic, and errors SHALL follow version/serial/IDs, algorithm/key, certificate-order extensions, then role/usage precedence.

#### Scenario: First deterministic semantic failure

- **WHEN** a certificate violates more than one admission rule
- **THEN** inspection returns the first error in the specified precedence with category, semantic reason, optional extension index, and certificate-DER byte offset

#### Scenario: Signature failure taxonomy

- **WHEN** signature metadata contains an unknown OID, recognized-OID parameter failure, or two differing individually admitted algorithms
- **THEN** inspection distinguishes `SignatureAlgorithm`, `SignatureParameters`, and `SignatureAlgorithmMismatch` respectively

#### Scenario: Resource budget boundary

- **WHEN** an interpreted extension value, extension count, SAN name count, constraint subtree count, TLV node count, or depth exceeds its configured inclusive limit
- **THEN** inspection returns `ResourceLimit` without allocation or unchecked arithmetic

### Requirement: Signature algorithms and issuer linkage use retained bytes exactly

Leaf and intermediate inspection SHALL require inner and outer signature AlgorithmIdentifiers to denote the same admitted effective ECDSA-P256/SHA-256, RSA-PSS SHA-256/MGF1-SHA-256/salt32/trailer1, or RSA-PKCS1-v1_5/SHA-256 algorithm. Admitted absent and NULL RSA spellings SHALL compare equivalent. `verifyIssuedBy` SHALL require exact complete subject issuer-DER equality with issuer subject-DER equality and SHALL verify the subject's original retained TBSCertificate bytes using the issuer's admitted key and existing primitives. The operation SHALL allocate nothing and MUST NOT imply validity dates, authority, a complete path, identity, or revocation.

#### Scenario: Valid issuer signature

- **WHEN** the exact issuer/subject DER linkage matches and an admitted issuer key verifies the subject’s original TBS bytes and signature metadata
- **THEN** `verifyIssuedBy` succeeds without re-encoding signed data

#### Scenario: Invalid signature cannot authenticate

- **WHEN** linkage differs, metadata differs, or a supported primitive rejects the signature
- **THEN** `verifyIssuedBy` returns an owned `ProfileError` and never converts the primitive failure into success

### Requirement: Extension policy implements the selected TLS-server subset

The profile SHALL implement the fixed BasicConstraints, KeyUsage, ExtendedKeyUsage, SAN, NameConstraints, CertificatePolicies, AKI, and SKI rules recorded by JUL-185. It SHALL validate every SAN DNS/IP identity, reject unsupported alternatives in critical SAN, require a nonempty critical SAN when the subject Name is empty, require critical nonempty DNS/IP-only NameConstraints on CAs, reject NameConstraints on end entities, and retain borrowed complete SAN/NC DER for downstream path and identity processing. An empty subject without a nonempty critical SAN SHALL report `EmptySubject`; malformed or empty SAN on a nonempty subject SHALL remain `SubjectAltName`, and `Role` SHALL remain reserved for CA and BasicConstraints policy. Unsupported NC forms SHALL fail rather than bypass subject-DN semantics.

#### Scenario: Role-specific CA semantics

- **WHEN** an intermediate or v3 anchor lacks `cA=true`, an intermediate has noncritical BC, or a required key usage is absent from a present KU/EKU
- **THEN** inspection rejects the candidate with the role/usage semantic reason

#### Scenario: Empty subject requires a nonempty critical SAN

- **WHEN** a certificate subject is empty and SAN is missing, noncritical, or critical but empty
- **THEN** inspection rejects it with `EmptySubject` without collapsing the failure into CA-role or generic SAN policy

#### Scenario: Name constraint profile

- **WHEN** a CA NameConstraints extension is critical, nonempty, bounded, and contains only canonical DNS or contiguous-mask IPv4/IPv6 subtrees
- **THEN** inspection accepts it as a borrowed semantic value for later cumulative path processing

### Requirement: Trust anchors are explicit independent owners

The standard library SHALL expose opaque `silk/trust_anchor.TrustAnchor`. `fromCertificate` SHALL infallibly move and retain the complete decoded certificate, including unsupported algorithms and unknown extensions, without validating self-signature, time, algorithm, or trust. `certificate` SHALL return a read-only borrow, and `encodedBytes` SHALL count retained certificate DER plus copied configured NameConstraints DER with overflow protection. `clone` SHALL create an independent owner, preserve every retained byte and configured restriction, use `OutOfMemoryError` with explicit mutable `Allocator`, and release partial storage on allocation failure.

#### Scenario: Explicit authority does not imply admissibility

- **WHEN** a caller constructs an anchor from a decoded unsupported certificate
- **THEN** construction succeeds and preserves the complete certificate, while later semantic inspection may reject that individual candidate

#### Scenario: Independent clone lifetime

- **WHEN** an anchor with configured bytes is cloned and either owner is then released
- **THEN** the remaining owner continues to expose identical certificate and configured bytes from independent storage

### Requirement: Configured constraints only narrow embedded policy

`fromCertificateWithConstraints` SHALL move the certificate, preflight and strictly validate at most one optional configured NameConstraints value under `ProfileLimits`, copy it only after preflight, and retain optional configured path length separately from embedded BC/pathLen/NC. Failure SHALL publish no anchor and SHALL release consumed or partial storage. The API SHALL expose immutable optional getters and SHALL provide no setter, hidden store, or OS trust-policy interpretation.

#### Scenario: Embedded and configured provenance is preserved

- **WHEN** an anchor certificate contains embedded constraints and construction also supplies configured path length or NameConstraints
- **THEN** both sources remain individually observable and later consumers can intersect them rather than replace one with the other

#### Scenario: Configured constraint failure is atomic

- **WHEN** configured constraint DER is malformed, unsupported, over budget, or allocation fails
- **THEN** construction returns the appropriate profile or allocation failure and no partial trust anchor escapes

### Requirement: Delivery evidence remains portable and economical

The implementation SHALL use ordinary Silk source with no compiler-known actor or crypto provider. Repository evidence SHALL reuse existing decoder, SAN, RSA, and ECDSA fixtures; pin selected Apache-2.0 x509-limbo cases with source IDs and hashes; preserve Zig HTTP comparison provenance at snapshot `1bc892110da738d6137b3f0b7e8e3a586ce09928` without treating it as a full-path oracle; include distinct native cases only for behavior not already falsified by neighboring tests; and include one compact LLVM-to-Wasm witness. Generated API/reference documentation SHALL state that constructing an anchor is a trust decision and semantic inspection is not authentication.

#### Scenario: Cross-target evidence

- **WHEN** the acceptance source is evaluated through the native corpus and the selected compact Wasm leg
- **THEN** supported views, semantic failures, ownership behavior, and portable ordinary-source execution agree with their fixed expectations

#### Scenario: Documentation avoids authentication claims

- **WHEN** a reader consults the generated or authored public reference
- **THEN** it distinguishes decoding, inspection, explicit anchor authority, issuer-signature validity, and complete path/identity authentication
