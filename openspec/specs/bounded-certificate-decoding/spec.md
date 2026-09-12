# bounded-certificate-decoding Specification

## Purpose

Provide a bounded structural certificate decoder whose independently owned outputs preserve every
byte and field required by later cryptographic, path, identity and trust-source consumers.

## Requirements

### Requirement: Explicit input formats return atomic owned results

The standard library SHALL expose the exact proposed Certificate.decodeDer, Certificate.decodePem
and CertificateBundle.decodePem signatures and owner-borrowed accessors in this change's design.
Inputs SHALL be borrowed bytes; outputs SHALL own their DER independently with no input lifetime.
A DER call SHALL consume exactly one certificate with no suffix. A single PEM call SHALL consume
exactly one block; a bundle SHALL consume one or more blocks and preserve order and duplicates.
No partial result SHALL escape. Allocation refusal SHALL use OutOfMemoryError with an explicit
mutable Allocator requirement; ordinary drop SHALL release all prior results and scratch.

#### Scenario: Input owner is released

- **WHEN** valid certificate input is decoded and the call completes successfully
- **THEN** the input may be released while output accessors continue to expose identical DER

#### Scenario: Later bundle member fails

- **WHEN** a bundle contains a valid certificate followed by a malformed certificate
- **THEN** the call returns only the failure and releases accumulated certificate storage

#### Scenario: Allocator refuses a later reservation

- **WHEN** allocation fails after earlier bundle members were decoded
- **THEN** OutOfMemoryError escapes without any prefix result or retained scratch allocation

### Requirement: A strict certificate PEM profile is explicit

PEM decoding SHALL follow the design's Input profile table: exact CERTIFICATE labels, matching
markers, standard base64 with canonical padding/pad bits, SP/HT/CR/LF whitespace and all three
newline conventions. It SHALL reject explanatory text, headers, other nonalphabet characters,
VT/FF, malformed markers and empty documents. Other well-formed labels SHALL be Unsupported.
It SHALL NOT skip blocks, auto-detect formats, repair input or claim universal RFC 7468 acceptance.
The documented RFC 7468 deviations SHALL accompany the eventual public API documentation.

#### Scenario: Bare CR and unusual wrapping

- **WHEN** a CERTIFICATE block uses bare CR newlines and arbitrary base64 wrapping with SP/HT
- **THEN** it decodes identically to its canonical LF representation within input limits

#### Scenario: Nonzero padding bits

- **WHEN** the final quartet has nonzero unused base64 pad bits
- **THEN** decoding returns Malformed.Base64 even if a permissive decoder would produce the same bytes

#### Scenario: Decorated or mixed bundle

- **WHEN** surrounding text or an unrelated PEM block appears in a bundle
- **THEN** the entire call fails instead of returning only selected certificates

### Requirement: Schema-aware DER is canonical within its declared boundary

The decoder SHALL enforce the design's DER and selected schema table for tags, lengths, containment,
INTEGER, BOOLEAN, BIT STRING, OID, NULL, RDN SET OF ordering, known DEFAULT fields, time forms,
sequence cardinality and certificate-version restrictions. It SHALL admit v1/v2/v3 and return
Unsupported.Version for a canonical nonnegative version above 2. It SHALL check bounded open-value
framing without inventing unknown schemas. Extension values, key bits and signature bits SHALL
remain opaque; the decoder SHALL NOT recursively validate them as known ASN.1 payloads.

#### Scenario: Explicit default or indefinite length

- **WHEN** a certificate encodes explicit default version zero or uses an indefinite DER length
- **THEN** it fails with the corresponding Malformed.DefaultValue or Malformed.Length outcome

#### Scenario: Unknown critical extension contains arbitrary bytes

- **WHEN** a structurally valid extension has an unknown OID, critical TRUE and payload FF
- **THEN** decoding succeeds and preserves the OID, critical bit and payload exactly

#### Scenario: Unordered relative distinguished name

- **WHEN** adjacent AttributeTypeAndValue encodings in an RDN SET OF are out of DER octet order
- **THEN** decoding fails with Malformed.SetOrder instead of sorting them

#### Scenario: Valid nonzero unused bit count

- **WHEN** a signature BIT STRING has unusedBits equal to one and its final low bit is zero
- **THEN** decoding preserves that count and payload without imposing verifier algorithm rules

### Requirement: Complete certificate information survives decoding

Outputs SHALL preserve complete original Certificate and signed TBSCertificate TLVs, version,
signed serial content, issuer/subject DER, validity DER and decoded dates, optional unique IDs,
complete SPKI and subjectPublicKey bits, signatureValue bits and unused-bit metadata, all three
AlgorithmIdentifiers with raw OIDs and absent/present parameter TLVs, and every extension's raw
encoding, OID, critical bit and value in source order. Unknown and duplicate extensions SHALL NOT
be discarded, merged or treated as trusted. Algorithms SHALL NOT be allowlisted by the decoder.

#### Scenario: Duplicate extension OIDs

- **WHEN** two structurally valid extensions use the same OID
- **THEN** both remain accessible in order with their original encodings and values

#### Scenario: Algorithms disagree

- **WHEN** inner and outer signature AlgorithmIdentifiers differ but each is structurally valid
- **THEN** both original encodings are returned so a verifier can reject the mismatch

#### Scenario: Policy-invalid serial or time interval

- **WHEN** a canonical signed serial is zero or negative, or valid time encodings form a reversed interval
- **THEN** decoding preserves them without claiming certificate validity

### Requirement: Finite independent budgets and typed failures are deterministic

The decoder SHALL use the exact inclusive DecodeLimits defaults and accounting in the design:
16 MiB input, 1 MiB per certificate, 8 MiB total DER, 1024 certificates, 256 KiB primitive/open
field content, 256 extensions per certificate, depth 32 and 65536 visited TLVs per certificate.
Zero SHALL mean zero. Checked arithmetic SHALL prevent overflow before indexing or allocation.
It SHALL return the declared Malformed, Unsupported or ResourceLimit reason, block index and offset
space under the specified left-to-right precedence; allocation remains a separate effect error.
The implementation SHALL use bounded traversal and linear scanning without recursive stack growth,
backtracking or duplicate-search amplification.

#### Scenario: Exact certificate byte boundary

- **WHEN** the pinned 560-byte fixture is decoded with certificateBytes 560 and other adequate limits
- **THEN** it succeeds, while certificateBytes 559 returns ResourceLimit.CertificateBytes

#### Scenario: Oversized truncated field

- **WHEN** an admissibly encoded declared length exceeds its field budget and available input
- **THEN** the field resource limit is reported before truncation, without reading outside input

#### Scenario: Limit zero

- **WHEN** a nonempty certificate is supplied with certificates zero
- **THEN** decoding returns ResourceLimit.Certificates before constructing a certificate

### Requirement: Conformance evidence and delivery status are honest

The follow-up SHALL exercise the pinned fixture manifest in this change, including provenance,
hashes, exact mutation recipes and expected structural/unsupported/limit results. It SHALL use
cheap structural/ownership checks and shared native acceptance cases; selected LLVM-to-Wasm
coverage SHALL establish intended portable behavior without redundant per-feature executables.
This design SHALL NOT be described as shipped parser support. Verification, path construction,
identity matching, trust acquisition and generic public ASN.1 tooling SHALL remain separate.

#### Scenario: Design-only delivery

- **WHEN** JUL-167's artifacts are published
- **THEN** the status states that the parser is planned and a separately estimated issue owns implementation

#### Scenario: Validator consumes decoded data

- **WHEN** a consumer receives a decoded certificate with an unknown critical extension
- **THEN** decoding success supplies raw evidence and makes no assertion that path or identity checks passed
