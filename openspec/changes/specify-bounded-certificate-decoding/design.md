## Context

Status: proposed contract only; no certificate parser ships in JUL-167. Admission at
`c6eae2f976a8f1a7681ffbfdcbbc8c776b9f0c96` inspected the delta from
`f5ada543a1738e4f0c69b311bdeb9a72e32e9dd3`. Graph discovery plus direct manifest,
Silk source, reference and active/archived OpenSpec review found no owning decoder.
Silk source is not fully indexed. Neighboring HMAC/HKDF, URI and compression actors now exist.
`Uri.parseOwned`, `Bytes.copy`, `Slice.view` and ordinary owned storage supply the API precedent.

## Goals / Non-Goals

Define a portable structural decoder that preserves information for independent validators.
Successful decoding means only that the selected envelope/schema is well formed and within limits.
It says nothing about trust, signatures, dates relative to now, identities or algorithm security.
No public generic ASN.1 tree, OS service, clock, network or cryptographic dependency is required.

## Decisions

### Public actors and ownership

The following are exact proposed declarations, with implementation bodies omitted, not runnable
examples or claims of installed modules. Imports use `silk.allocator`, `silk.result`,
`silk.option`, `silk.certificate` and `silk.certificate_bundle`. `Allocator`,
`OutOfMemoryError`, `Result` and `Option` retain their existing definitions.

```silk
// silk.certificate, inside impl Certificate
pub effect fn decodeDer(input: &[u8], limits: DecodeLimits) -> Result<Certificate, DecodeError>
! OutOfMemoryError ? &mut Allocator
pub effect fn decodePem(input: &[u8], limits: DecodeLimits) -> Result<Certificate, DecodeError>
! OutOfMemoryError ? &mut Allocator
pub fn der(self: &Self) -> &[u8]
pub fn tbsDer(self: &Self) -> &[u8]
pub fn version(self: &Self) -> CertificateVersion
pub fn serial(self: &Self) -> &[u8]
pub fn issuerDer(self: &Self) -> &[u8]
pub fn subjectDer(self: &Self) -> &[u8]
pub fn notBefore(self: &Self) -> CertificateTime
pub fn notAfter(self: &Self) -> CertificateTime
pub fn validityDer(self: &Self) -> &[u8]
pub fn spkiDer(self: &Self) -> &[u8]
pub fn publicKey<'a>(self: &'a Self) -> BitStringView<'a>
pub fn publicKeyAlgorithm<'a>(self: &'a Self) -> AlgorithmView<'a>
pub fn tbsSignatureAlgorithm<'a>(self: &'a Self) -> AlgorithmView<'a>
pub fn signatureAlgorithm<'a>(self: &'a Self) -> AlgorithmView<'a>
pub fn signature<'a>(self: &'a Self) -> BitStringView<'a>
pub fn issuerUniqueId<'a>(self: &'a Self) -> Option<BitStringView<'a>>
pub fn subjectUniqueId<'a>(self: &'a Self) -> Option<BitStringView<'a>>
pub fn extensionCount(self: &Self) -> usize
pub fn extension<'a>(self: &'a Self, index: usize) -> Option<ExtensionView<'a>>

// silk.certificate_bundle, inside impl CertificateBundle
pub effect fn decodePem(input: &[u8], limits: DecodeLimits) -> Result<CertificateBundle, DecodeError>
! OutOfMemoryError ? &mut Allocator
pub fn length(self: &Self) -> usize
pub fn get<'a>(self: &'a Self, index: usize) -> Option<&'a Certificate>
```

`Certificate` and `CertificateBundle` are opaque, move-only owners with private storage and no
public mutators or unchecked constructors. They store owned DER and checked offsets, never
self-references. The effect borrows input until execution completes or is dropped; outputs and
errors retain no input loan. The caller may then mutate/drop input. Returned views borrow the
owner, allocate nothing, and cannot outlive it. `get`/`extension` return `None` out of range.
A bundle retains input order and duplicate certificates; it does not imply chain order or trust.
No partial certificate, callback, iterator yield or prefix bundle escapes on failure.
Ordinary owned cleanup releases scratch and prior certificates on structured exits, including
allocation failure; no cleanup guarantee is invented for process termination or unrecoverable traps.

```silk
pub enum CertificateVersion { V1, V2, V3 }
pub struct CertificateTime {
  pub year: u16
  pub month: u8
  pub day: u8
  pub hour: u8
  pub minute: u8
  pub second: u8
}
pub struct BitStringView<'a> {
  pub bytes: &'a [u8]
  pub unusedBits: u8
}
pub struct AlgorithmView<'a> {
  pub der: &'a [u8]
  pub oid: &'a [u8]
  pub parametersDer: Option<&'a [u8]>
}
pub struct ExtensionView<'a> {
  pub der: &'a [u8]
  pub oid: &'a [u8]
  pub critical: bool
  pub value: &'a [u8]
}
```

All `der` views include tag and length; `tbsDer` is the original complete TBSCertificate TLV,
never reconstructed. `serial` is the original minimal signed INTEGER content, including any
necessary sign octet. OIDs are canonical DER content octets (no tag/length), not narrowed machine
integers or lossy text. Parameters preserve absence versus explicit NULL and the complete TLV.
BIT STRING views exclude the initial unused-bit-count octet but retain its value separately.
All three AlgorithmIdentifiers remain distinct. Every extension is retained in original order,
including unknown and critical ones; values are exact OCTET STRING content. Names remain full
RDNSequence DER, preserving RDN boundaries, attribute OIDs, value types and encoded values.
No string normalization, common-name extraction, SAN selection or key algorithm parsing occurs.

### Input profile

All rejection below is `Malformed` unless explicitly `Unsupported` or `ResourceLimit`.

| Input                                                                             | Outcome                             |
| --------------------------------------------------------------------------------- | ----------------------------------- |
| One exact DER Certificate TLV                                                     | Accept                              |
| Empty DER, concatenated DER, trailing byte/whitespace                             | Reject                              |
| v1 (version absent), v2 (INTEGER 1), v3 (INTEGER 2)                               | Accept                              |
| Explicit default v1 INTEGER 0                                                     | Reject noncanonical DEFAULT         |
| Negative version                                                                  | Reject                              |
| Canonical nonnegative version above 2                                             | Unsupported.Version                 |
| v1 unique IDs; v1/v2 extensions                                                   | Reject schema violation             |
| v2/v3 unique IDs; v3 extensions absent                                            | Accept                              |
| Exact case-sensitive CERTIFICATE markers                                          | Accept                              |
| X509 CERTIFICATE, TRUSTED CERTIFICATE, PUBLIC KEY or any other well-formed label  | Unsupported.PemLabel; never skip    |
| Mismatched, missing, malformed or nested markers                                  | Reject                              |
| LF, CRLF, bare CR, including mixed newline styles                                 | Accept                              |
| SP/HT around markers on their otherwise empty lines                               | Accept                              |
| SP/HT/CR/LF between blocks or before/after the document                           | Accept                              |
| SP/HT/CR/LF anywhere within base64                                                | Ignore; any line width accepted     |
| VT, FF, BOM, NUL, non-ASCII or other nonalphabet content                          | Reject                              |
| Preamble, explanatory text, PEM headers, comments, unrelated blocks               | Reject                              |
| Missing newline after BEGIN or before END                                         | Reject                              |
| END at EOF without final newline                                                  | Accept                              |
| Empty base64 block, empty/whitespace-only PEM document                            | Reject                              |
| Standard base64 alphabet, exact final padding, zero pad bits                      | Accept                              |
| Missing required padding, excess/interior padding, URL alphabet, nonzero pad bits | Reject                              |
| Full final quartet needing no padding                                             | Accept                              |
| Two or more valid blocks in Certificate.decodePem                                 | Reject TrailingData at second BEGIN |
| One or more blocks in CertificateBundle.decodePem                                 | Accept within count/byte bounds     |
| Later invalid block in bundle                                                     | Entire call fails; no prefix result |

Markers have exactly five hyphens at each end and exactly one space between BEGIN/END and label.
After ignoring allowed body whitespace, base64 length is a multiple of four; only its last
quartet may use one or two `=` as dictated by the decoded length. Each block decodes exactly one
DER certificate, with no decoded suffix. No automatic DER/PEM detection or recovery scanning.

This is a deliberate strict subset of [RFC 7468 §§2–3,5](https://www.rfc-editor.org/rfc/rfc7468.html):
it rejects surrounding explanatory text and non-base64 characters rather than ignoring them,
rejects VT/FF, requires matching labels, and requires DER although textual certificates can
carry BER. It supports all three newline conventions. It does not claim universal RFC 7468
parser acceptance. A trust-source provider needing decorated bundles must explicitly adapt its
input policy; the decoder never silently salvages a subset.

### DER and selected schema

Authority: [X.690 (02/2021)](https://www.itu.int/rec/T-REC-X.690-202102-I/en),
including [Erratum 1 (09/2021)](https://www.itu.int/rec/T-REC-X.690-202109-I!Err1/en),
§§8,10,11, and [RFC 5280 §4.1 and Appendix A.1](https://www.rfc-editor.org/rfc/rfc5280.html#section-4.1).
The decoder recognizes the Certificate/TBSCertificate schema and Name/RDN/AttributeTypeAndValue,
Validity, SubjectPublicKeyInfo, AlgorithmIdentifier and Extension envelopes. It does not interpret
extension OCTET STRING payloads, SPKI BIT STRING payloads or signature BIT STRING payloads as ASN.1.

| Rule        | Accepted / rejected boundary                                                                                                                                                                        |
| ----------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Tags        | Correct class, number and primitive/constructed bit for every known field; shortest identifier; no EOC; high-tag form must use nonzero first base-128 group and encode a number >=31                |
| Length      | Definite only; shortest form (short below 128); no leading zero length octet, indefinite form or reserved FF length octet                                                                           |
| Containment | Every child fits its parent; mandatory fields, exact sequence order, no duplicate singleton fields, no unconsumed child/suffix                                                                      |
| INTEGER     | Nonempty minimal two's-complement; no redundant 00/FF; serial sign/size retained without positivity/20-octet Web PKI policy                                                                         |
| BOOLEAN     | Exactly one content octet, 00 or FF; explicit extension critical FALSE is rejected because DEFAULT FALSE must be absent                                                                             |
| BIT STRING  | Primitive; count 0..7; zero unused tail bits; empty payload requires count zero; do not force count zero for keys/signatures at decoding layer                                                      |
| OID         | Nonempty; each subidentifier terminates; no leading 80 base-128 group; first combined subidentifier decoded conceptually into arcs 0/1/2; no u32/u64 arc restriction                                |
| NULL        | Primitive with zero content octets                                                                                                                                                                  |
| SET OF      | RDN members ordered lexicographically by complete DER TLV octets (unsigned); equal encodings allowed; no sorting or rewriting                                                                       |
| DEFAULT     | Known schema defaults omitted (version v1, critical FALSE); no guessed defaults in open types                                                                                                       |
| Time        | UTCTime YYMMDDhhmmssZ for 1950..2049; GeneralizedTime YYYYMMDDhhmmssZ for 0001..9999; Gregorian valid date, seconds 00..59, no offset/fraction/omitted seconds; reject year zero and alternate tags |
| Names       | SEQUENCE OF nonempty SET OF attribute SEQUENCE {OID, exactly one value TLV}; empty overall issuer/subject retained for later policy                                                                 |
| Extensions  | Optional explicit [3] containing nonempty SEQUENCE OF Extension; duplicate OIDs retained, not collapsed/rejected by this decoder                                                                    |
| Unique IDs  | Optional primitive implicit [1]/[2] BIT STRING contents in schema order, preserved                                                                                                                  |
| Algorithms  | SEQUENCE {OID, optional one parameter TLV}; absent/NULL distinct; no OID allowlist or inner/outer algorithm equality check                                                                          |

Open parameter and attribute values receive bounded TLV framing checks. Constructed values are
walked for canonical child framing; recognizable universal INTEGER, BOOLEAN, BIT STRING, OID,
NULL and string encodings receive their universal canonical checks. Universal UTF8String must
encode Unicode scalars without overlong forms; PrintableString uses its ASN.1 alphabet, IA5String
is ASCII, BMPString is even-length UCS-2 without surrogates, UniversalString contains valid scalar
values in four-byte big-endian units. Other primitive open values are opaque. Recognized universal tags enforce their primitive/constructed bit even in open values: SEQUENCE
and SET are constructed; INTEGER, BOOLEAN, BIT STRING, OID, NULL and DER primitive strings are
primitive. For unknown tags, validate identifier/length and framing
without inventing a schema, DEFAULT rule, implicit content type or SET-vs-SET-OF order. In
particular an unknown universal SET open value has no schema to choose ordering rules; its children
are framed only. This is not a claim to certify canonical encoding of arbitrary open ASN.1 types.
Extension payloads remain opaque even for familiar extension OIDs: an unknown critical extension
with payload FF is retained, not parsed as malformed DER.

The time profile excludes fractional GeneralizedTime permitted by DER, following RFC 5280
certificate time syntax. Canonical GeneralizedTime before 2050 is accepted by this decoder;
issuance year/tag policy belongs to the validator.

Duplicate extensions, zero/negative/long serials, empty names, reversed validity, algorithm
mismatch, unknown algorithms and unknown critical extensions all survive decoding if structurally
valid and within budgets. RFC 5280 validation/algorithm policy must decide their acceptability.
This preserves evidence instead of silently weakening or preselecting a future validator's rules.

### Finite resources and errors

```silk
pub struct DecodeLimits {
  pub inputBytes: usize
  pub certificateBytes: usize
  pub totalDerBytes: usize
  pub certificates: usize
  pub fieldBytes: usize
  pub extensions: usize
  pub depth: usize
  pub nodes: usize
}
// inside impl DecodeLimits
pub fn defaults() -> DecodeLimits
pub enum DecodeClass { Malformed, Unsupported, ResourceLimit }
pub enum DecodeReason {
  EmptyInput, PemSyntax, Base64, TrailingData, Truncated, Tag, Length,
  Integer, Boolean, BitString, Oid, SetOrder, DefaultValue, Time, Schema,
  StringEncoding, Version, PemLabel, InputBytes, CertificateBytes,
  TotalDerBytes, Certificates, FieldBytes, Extensions, Depth, Nodes, SizeOverflow,
}
pub enum DecodeOffsetSpace { Input, Der }
pub struct DecodeError {
  pub kind: DecodeClass
  pub reason: DecodeReason
  pub certificateIndex: usize
  pub offsetSpace: DecodeOffsetSpace
  pub offset: usize
}
```

`DecodeLimits` is a Copy value. All limits are inclusive and independent, zero means zero (never
unlimited). No sentinel, implicit budget growth or unchecked platform conversion exists.

| Limit            |  Default | Accounting                                                                                                                                                                                                         |
| ---------------- | -------: | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| inputBytes       | 16777216 | Entire supplied input including PEM decoration/whitespace                                                                                                                                                          |
| certificateBytes |  1048576 | Complete DER TLV for each certificate                                                                                                                                                                              |
| totalDerBytes    |  8388608 | Sum of DER lengths across the call, including duplicates                                                                                                                                                           |
| certificates     |     1024 | Certificates in call, including single-certificate APIs                                                                                                                                                            |
| fieldBytes       |   262144 | Content length of every primitive field and every open value (including constructed parameters/attribute values); includes BIT STRING unused-count octet; other constructed schema containers use certificateBytes |
| extensions       |      256 | Per-certificate extension count, duplicates included                                                                                                                                                               |
| depth            |       32 | Traversed TLVs, Certificate root depth 1; opaque octets/bits do not add depth                                                                                                                                      |
| nodes            |    65536 | Total visited TLVs per certificate, including open-value descendants; opaque bytes do not add nodes                                                                                                                |

Use subtraction-before-addition bounds checks for offsets and checked arithmetic for allocation,
base64 sizing, counts and totals. A nonrepresentable decoded length/tag number or accounting total
returns ResourceLimit.SizeOverflow, never wraps or truncates. Arbitrarily large OID subidentifiers
are compared/retained as bytes rather than accumulated in a machine integer. End-of-buffer inside
an otherwise admissible field is Malformed.Truncated, including missing length/content/base64
quartet/END. No cursor advances past a checked parent boundary.

Error precedence is deterministic: check entire inputBytes first; then parse left to right,
checking certificate count before each new block. At each TLV: identifier/length syntax and
representability, field/certificate size budgets, parent containment, depth/node budget, then
schema/content. Aggregate DER budget is checked from the known certificate size before decoding
its contents. Thus a declared oversized but truncated field reports its size limit first.
A single PEM decoder reports TrailingData on a second BEGIN instead of inspecting its contents.
Version/PemLabel are the only Unsupported reasons; budget reasons map only to ResourceLimit;
all remaining reasons map to Malformed. Missing data points at the relevant end offset; bad
encoding points at the first offending octet, semantic TLV violations at that TLV's tag.
PEM lexical errors use Input offsets; decoded DER errors use Der offsets relative to that block.
DER entry-point errors use Input offsets. The zero-based certificateIndex is the attempted block
index (zero for single calls or document-level errors). Allocation refusal uses only the existing
`OutOfMemoryError` effect channel, not DecodeError, and can precede defects not yet visited.

Implementation uses a bounded explicit traversal stack and monotonic scanning, no recursive native
stack growth, backtracking or quadratic duplicate detection. Comparing adjacent RDN encodings is
linear in their total bytes; extension duplicates need no scan because all are retained. Fixed
passes plus bounded per-node bookkeeping give O(inputBytes + totalDerBytes + total nodes) work
and O(totalDerBytes + total nodes + depth) owned/scratch storage. Allocation lengths are checked;
allocator metadata is not falsely promised to fit a byte-exact portable budget.

### Consumer coordination

- [JUL-166](https://linear.app/juliaortiz/issue/JUL-166): pass original signed TLV, both signature AlgorithmIdentifiers, complete SPKI and signature bits. Its selected ECDSA/RSA policy does not restrict decoding; its verifier checks key/signature encoding, parameters and algorithm agreement.
- [JUL-168](https://linear.app/juliaortiz/issue/JUL-168): consume preserved names, serial, dates, version and ordered raw extensions; reject duplicate/unknown critical extensions and apply policy in that layer. Decoding success carries no trusted flag.
- [JUL-169](https://linear.app/juliaortiz/issue/JUL-169): obtain SAN OCTET STRING content by OID from retained extensions. Its bounded GeneralNames decoding/matching contract owns SAN semantics, malformed entries and duplicate SAN policy; this slice exposes no lossy DNS/IP list.
- [JUL-170](https://linear.app/juliaortiz/issue/JUL-170): provide bytes explicitly, receive an atomic owned bundle, and choose acquisition, caching, deduplication and trust metadata separately. No input is silently skipped.

These boundaries are usable before consumer implementations exist. The design is portable across
native and LLVM-to-Wasm with an explicit allocator; no retired evaluator/direct-Wasm support is
claimed. See [fixtures.md](fixtures.md) and [implementation.md](implementation.md).

## Risks / Trade-offs

- Strict PEM rejects decorated root files → explicit provider policy and named deviations, no salvage fallback.
- Owned copies increase storage → finite aggregate budgets, exact reservation where available, simple immutable lifetime contract.
- Structural success can be mistaken for Web PKI validity → distinct decoder errors and consumer responsibilities; never expose a validation flag.
- Open ASN.1 types lack schemas → preserve encodings and limit checks honestly; algorithm/extension consumers enforce their own schemas.

## Migration Plan

No current certificate API exists to migrate. Publish this design and follow-up; implement ordinary
source actors in that follow-up, regenerate manifest/docs, and run the required verification before
claiming availability. Rollback of this design removes planning artifacts only.
