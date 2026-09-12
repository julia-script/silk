## Context

This contract originated as the design deliverable for [JUL-169](https://linear.app/juliaortiz/issue/JUL-169).
Julia subsequently authorized its runtime implementation, including JUL-183 and JUL-184. See [proposal.md](proposal.md) for motivation and
[the delta specification](specs/https-service-identity/spec.md) for observable requirements.

Initial work admission inspected clean `c6eae2f976a8f1a7681ffbfdcbbc8c776b9f0c96` against
`f5ada543a1738e4f0c69b311bdeb9a72e32e9dd3`. URI, HMAC/HKDF, inflate/zstd and storage APIs have
landed since triage. Graph queries and bounded source/manifest/reference/OpenSpec searches found
no identity implementation; Silk graph coverage is incomplete. Open PR #404 owns cryptographic
profile selection, not identity. Existing `Uri.host` preserves brackets and percent escapes;
`Uri.hostKind` distinguishes registered names, IPv4, IPv6 and IPvFuture. It deliberately does not
establish an HTTPS reference identity.

Rebase admission additionally inspected that work base through
`991386ae75fe3037e70da1cde9dc71d91dbc3e67`. Merged PR #405 specifies bounded certificate
decoding (JUL-167); PR #407 implements it (JUL-182) and repairs fourteen baseline native corpus
fixtures. The delivered certificate actor preserves raw extensions, including duplicate OIDs,
without decoding GeneralNames or matching identities. This does not supersede JUL-169. The
matcher remains independent of that actor; the missing SAN adapter is identified below.

## Goals / Non-Goals

**Goals:** An allocation-free, target-neutral matcher over explicit data; one origin reference;
repeatable error precedence; bounded validation before success; an independently implementable API.

**Non-Goals:** DER/PEM parsing, certificate signatures or chain trust, revocation, name constraints,
DNS lookup, reverse DNS, public-suffix lookup, Unicode/IDNA conversion, general URI parsing,
TLS state, SNI policy, HTTP redirects or connection coalescing. Matching alone never authorizes
application data. No compiler privilege, service or allocator requirement is needed.

## Decisions

### 1. Explicit origin and decoded-input boundaries

The matcher module is `silk.https_identity`. Its actor is `HttpsIdentity`; operations are inherent
members. The following are the public data declarations; the implementation also declares Copy for these small borrowed/scalar values. All payload bytes remain ordinary
untrusted data. Public construction does not certify validity; `verify` rechecks its inputs.

```silk
import silk.result {Result}
import silk.u8
import silk.usize

pub enum UnsupportedForm {
  IpvFuture,
  ZoneIdentifier,
  UriId,
  SrvId,
}

pub union OriginHost<'name> {
  Dns { pub bytes: &'name [u8] },
  Ipv4 { pub bytes: [u8; 4] },
  Ipv6 { pub bytes: [u8; 16] },
  Unsupported { pub form: UnsupportedForm },
}

pub union ReferenceIdentity<'name> {
  Dns { pub bytes: &'name [u8] },
  Ipv4 { pub bytes: [u8; 4] },
  Ipv6 { pub bytes: [u8; 16] },
}

pub union PresentedIdentity<'cert> {
  Dns { pub bytes: &'cert [u8] },
  Ip { pub bytes: &'cert [u8] },
  Other { pub tag: u8, pub bytes: &'cert [u8] },
}

pub enum CertificateStructureError {
  InvalidDer,
  InvalidGeneralName,
  DuplicateSanExtension,
  EmptySanExtension,
}

pub union CertificateIdentities<'cert> {
  Absent,
  Decoded { pub entries: &'cert [PresentedIdentity<'cert>] },
  Malformed { pub reason: CertificateStructureError },
}

pub enum NameError {
  EmptyName,
  NameTooLong,
  EmptyLabel,
  LabelTooLong,
  Nul,
  NonAscii,
  InvalidCharacter,
  EdgeHyphen,
  TrailingDot,
  WildcardReference,
  NumericFinalLabel,
  MissingWildcardSuffix,
}

pub enum CertificateIdentityError {
  InvalidIpLength,
  WrongGeneralNameTag,
}

pub enum LimitKind {
  SanCount,
  SanBytes,
}

pub union IdentityError {
  MalformedReference { pub reason: NameError },
  MalformedCertificateStructure { pub reason: CertificateStructureError },
  MalformedCertificateName { pub index: usize, pub reason: NameError },
  MalformedCertificateIdentity { pub index: usize, pub reason: CertificateIdentityError },
  Unsupported { pub form: UnsupportedForm },
  LimitExceeded { pub kind: LimitKind },
  NoMatch,
}

pub struct IdentityLimits {
  pub maxSanCount: usize
  pub maxSanBytes: usize
}

pub struct IdentityMatch {
  pub sanIndex: usize
}

pub struct HttpsIdentity {}
```

Exact operation headers (implementation bodies live in the stdlib source):

```silk
// In impl HttpsIdentity:
pub fn reference<'name>(host: OriginHost<'name>) -> Result<ReferenceIdentity<'name>, IdentityError>
pub fn verify<'name, 'cert>(
  reference: &ReferenceIdentity<'name>,
  certificate: &CertificateIdentities<'cert>,
  limits: IdentityLimits,
) -> Result<IdentityMatch, IdentityError>

// In impl IdentityLimits:
pub fn standard() -> IdentityLimits
```

`reference` consumes only the small origin value. DNS bytes are borrowed unchanged with `'name`;
IP arrays move into the reference and own their four/sixteen network-order octets. `verify` retains
neither reference nor certificate; success and errors own only scalar metadata. `'cert` bounds
both the SAN descriptor slice and its payloads. The caller owns these buffers and cannot mutate
them during shared borrowing. No hidden certificate copy, self-reference, cached trust decision,
allocation, I/O, Effect failure row or provider is involved. Callers needing longer-lived DNS
references keep their own `String`/`Bytes` owner and create a view when needed.

This shape avoids coupling to certificate handles or inventing an owned certificate model. A
caller can populate descriptors from static bytes, as the delivered matcher fixtures do. Production
adapters must preserve the complete leaf SAN list in source order and report structural failure;
they must not filter names before this boundary. `Absent` means no SAN extension, not an empty
present extension. `Decoded` with zero entries is `EmptySanExtension`. `Other.tag` is the decoded
GeneralName alternative number 0..8 excluding 2 (dNSName) and 7 (iPAddress); those two must use
`Dns`/`Ip`. Out-of-range or misclassified tags produce `WrongGeneralNameTag`. URI-ID uses tag 6;
SRVName is within tag 0. No semantic parsing of `Other` payloads occurs here. Its bytes include the
complete alternative content octets, including nested otherName content, for accounting.

The delivered `silk.certificate.Certificate` supplies `extensionCount(self: &Self) -> usize` and
`extension<'a>(self: &'a Self, index: usize) -> Option<ExtensionView<'a>>`.
`ExtensionView.oid` holds DER OID content octets, and `value` holds the opaque extension OCTET
STRING content. Both borrow the certificate owner. The decoder neither interprets that value
as GeneralNames nor rejects duplicate extension OIDs; its success cannot be substituted for
`CertificateIdentities.Decoded`.

The SAN adapter must select subjectAltName OID `2.5.29.17` (content octets
`55 1d 11`), detect duplicate occurrences across the complete extension list, and decode its
GeneralNames content before constructing these descriptors. It must distinguish absence from an
empty or malformed value, preserve order and every alternative, and retain the certificate owner
and descriptor storage for the shared borrow. Its own raw-input, traversal and descriptor-storage
budgets must apply before matching; the matcher's payload budgets do not bound ASN.1 decoding.
The accepted JUL-167 design assigns SAN interpretation to the identity consumer, not the envelope
decoder. This adapter is tracked separately as [JUL-184](https://linear.app/juliaortiz/issue/JUL-184)
(five-point implementation slice), and is now included by explicit user authorization.
Manually supplied decoded inputs remain sufficient for the pure matcher.

### 2. Select the original HTTPS host before networking

The HTTPS caller takes the host from the original origin being authenticated, before resolving it
or accepting certificate names. RFC 3986 first-match host classification selects IPv4 before
registered-name handling; an IPv6 literal selects IPv6. The caller decodes those literals to
`OriginHost.Ipv4`/`Ipv6`; every other admitted host selects `Dns`. The adapter checks the `https`
scheme and never passes userinfo, port, path, query or fragment. Port remains part of HTTP origin
isolation even though it is absent from this certificate-name comparison.

Brackets delimit an IPv6 URI host and are removed only by the caller after successful syntax
validation, before conversion to sixteen bytes. They are not accepted as DNS bytes. This module
has no textual IP parser. The caller must reject malformed IP syntax rather than reinterpret a
failed IPv6 literal as DNS. It must reject IPvFuture and zone identifiers (including `%25` zone
syntax) with `Unsupported` before attempting conversion; the explicit `OriginHost.Unsupported`
arm allows reporting this without a URI implementation. Passing URI-ID or SRV-ID reference
requests through that arm also yields `Unsupported`. Unsupported SAN forms merely do not match.

First-slice HTTPS admission rejects all percent-encoded origin host spellings; it does not decode
one spelling for routing and compare another for authentication. DNS host bytes include neither
brackets nor a port. Generic URI parsing remains broader and unchanged. DNS search suffixes,
CNAME/DNAME targets, SVCB/HTTPS alternative targets, reverse DNS and resolved addresses never
replace the reference. Redirects require the HTTP owner to construct a new reference for the new
origin. No certificate input can change reference construction.

### 3. DNS admission is deliberately narrower than generic registered names

DNS inputs have 1..253 octets without a final root dot, with labels of 1..63 octets separated by
literal `.`. Each non-wildcard label uses ASCII letters, digits and hyphens, starting and ending
with a letter or digit. Empty names/labels, a trailing dot (including multiple dots), NUL,
non-ASCII, underscores, whitespace, delimiters, escapes and reference wildcards are rejected.
The final label cannot be all decimal digits. This local HTTPS admission rule excludes dotted
numeric forms that different network APIs could interpret as IP addresses, including malformed
dotted quads, short IPv4 and integer IPv4. In particular a dotted IPv4 supplied as `Dns` cannot
accidentally authenticate against textual dNSName; the caller must supply its binary IP form.

Inputs are absolute DNS identities: a single label is allowed as an absolute name, never a request
to apply a search suffix. No resolver or public suffix policy is consulted. One trailing dot is
rejected, not silently stripped; applications must reject the same spelling for connection setup
in this first profile. Do not silently normalize only the verification input.

ASCII A-label spellings such as `xn--bcher-kva.example` are compared directly. This slice validates
the ASCII label grammar, not Punycode decoding, IDNA round trips or Unicode contextual rules. A
caller presenting internationalized names must supply already prepared A-labels; raw U-labels are
rejected as `NonAscii`. No Unicode normalization, UTS #46 mapping or Unicode-to-IDNA conversion is
performed. An LDH string with an `xn--` prefix is not certified as valid IDNA by matcher success.

Malformed-reference reason precedence is: empty name, total length, first NUL, first non-ASCII,
trailing dot, any `*`, then labels left to right (empty, oversized, disallowed character, edge
hyphen), then numeric final label. No byte offset is promised; errors return the reason only.

### 4. Validate the whole SAN set, then compare eligible identifiers

Error precedence for `verify` is fixed:

1. Revalidate a directly constructed DNS reference with the same rules as `reference`.
2. Reject the certificate's explicit structural error, or a zero-length `Decoded` list.
3. Check total SAN count, then sum every descriptor's payload length with checked arithmetic.
   An overflow is `LimitExceeded.SanBytes`. Check limits before scanning payload bytes.
4. Validate all entries in order. Return the first malformed entry's index/reason. Validate both
   supported forms even when they differ from the reference form. No successful name hides a
   later malformed entry.
5. Search valid eligible SANs in source order and return the first matching `sanIndex`. Otherwise
   return `NoMatch`, including absent SAN/CN-only and unsupported-only SAN sets.

For DNS SANs, non-ASCII octets invalidate IA5String and fail with a malformed certificate name
before wildcard handling. NUL is an IA5-valid octet but malformed DNS syntax, not a DER failure.
For remaining ASCII DNS payloads, count `*`: if there is more than one, or its sole
occurrence is not the entire leftmost label, ignore that identifier as RFC 9525 §6.3 requires.
Continue considering other entries. Such an ignored identifier still counts toward all limits.
After this special rule, validate the ordinary DNS grammar above, including NUL rejection;
the allowed initial `*` label
is exempt from LDH checks but requires a nonempty suffix (`*` alone is
`MissingWildcardSuffix`). Other invalid names fail the complete set. Reference-only
`WildcardReference` never describes a SAN; otherwise name-error precedence is the reference
order after the non-ASCII/wildcard checks, with missing wildcard suffix before label checks.
Thus `w*` plus NUL is ignored, while `*.exa` plus NUL plus `mple` fails as `Nul`. All scanning is
length-delimited; no C-string truncation is permitted.

Case folding affects ASCII A..Z only. Exact DNS matches require equal labels and label count.
An eligible `*.example` matches one nonempty leftmost label, including an A-label, followed by
exactly `example`; it matches neither `example` nor `a.b.example`. `*.com` is not suppressed by
an implicit public-suffix rule. Partial-label matching, regexes and backtracking are excluded.
CN and all other subject RDNs are unavailable to the API and never used as fallback.

The strict policy for other malformed SAN names is intentional: accepting them would complicate
auditing and downstream decoder contracts. Structural invalidity is never an ignorable wildcard
failure. This is a local admission choice beyond the RFC's mandatory invalid-wildcard skipping.

### 5. IP matching uses exact binary family and address equality

An `Ip` SAN must contain exactly 4 or 16 octets, otherwise the entire set fails with
`MalformedCertificateIdentity.InvalidIpLength`. Equality requires the same length and identical
network-order bytes. Four-byte IPv4 never equals IPv4-mapped sixteen-byte IPv6. IPv6 textual
compression, case and embedded IPv4 spelling are caller conversion concerns; equal decoded
sixteen-byte values match. Neither textual dNSName nor URI-ID/SRV-ID can match an IP reference.
No subnet, mask or prefix matching is allowed.

RFC 5280 name constraints belong solely to JUL-168: their DNS subtree and 8/32-byte IP
address-plus-mask representations are not leaf identities. Feeding such IP lengths as leaf SANs
is malformed. Identity success says nothing about constraints, certificate validity, issuer trust,
signature verification, EKU or revocation. TLS must require identity and trust success for the
same leaf certificate, plus its own handshake authentication.

### 6. Fixed default limits and linear work

`IdentityLimits.standard()` returns `maxSanCount = 256`, `maxSanBytes = 65536`. All caller-supplied
values are valid budgets, including zero; there is no implicit unlimited sentinel. Count includes
every SAN, including unsupported and ignored forms. Bytes is the sum of payload lengths in the
descriptors, not the DER certificate size or descriptor storage size. Aliased/repeated payloads
are counted on each occurrence. No payload is inspected once its total exceeds the budget.
Structural DER decoding has its own earlier JUL-167 bounds, which this API cannot enforce.

With reference length R <= 253, N entries and B total payload bytes, work is O(R + N + B):
validate lengths/count in O(N), scan supported payloads once for syntax and once for comparison.
Never rescan the reference once per SAN without a corresponding candidate length/suffix bound;
an implementation can compare only equal-length DNS candidates (wildcard suffix comparison is
bounded by that candidate's bytes). Temporary storage is O(1), no allocation or recursion, and
the successful result includes no list of matched or rejected names. The count and byte budgets
therefore bound the work without a platform-dependent instruction-count quota.

### 7. Authorities and implementation comparison

Normative editions: RFC 9525 (November 2023), RFC 9110 (June 2022), RFC 5280 (May 2008),
RFC 3986 (January 2005). RFC 9110 references RFC 6125; this design applies its successor RFC 9525.

- [RFC 9110 §§4.3.4–4.3.5](https://www.rfc-editor.org/rfc/rfc9110.html#section-4.3.4)
  supplies the HTTPS host-to-identifier mapping and binary IP input boundary.
- [RFC 9525 §§4.2 and 6](https://www.rfc-editor.org/rfc/rfc9525.html#section-6)
  supplies origin-independent reference construction, DNS/IP matching and invalid-wildcard
  skipping. Its §4.2 explicitly excludes URI-ID certificate use for HTTP; §2 excludes CN.
- [RFC 5280 §§4.2.1.6 and 4.2.1.10](https://www.rfc-editor.org/rfc/rfc5280.html#section-4.2.1.6)
  separates SAN encodings from name constraints. DER and trust are separate owners.
- [RFC 3986 §3.2.2](https://www.rfc-editor.org/rfc/rfc3986.html#section-3.2.2)
  supplies upstream literal classification. The generic URI parser's admission is broader than
  this HTTPS profile.

Comparison is pinned to rustls/webpki commit `3d0adc46704c1d985a9fefe2034650488d96d49e`,
not treated as the normative oracle:

| Surface                                                                                                                     | Pinned webpki behavior                                           | Selected Silk policy                                                                         |
| --------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------- | -------------------------------------------------------------------------------------------- |
| [DNS matcher](https://github.com/rustls/webpki/blob/3d0adc46704c1d985a9fefe2034650488d96d49e/src/subject_name/dns_name.rs)  | Skips `MalformedDnsIdentifier`; permits underscores              | Skip the RFC-invalid wildcard forms; otherwise reject malformed names, including underscores |
| SAN iteration                                                                                                               | `find_map` may stop at a match before a later structural failure | Complete structure and supported-name validation before matching                             |
| Wildcard suffix                                                                                                             | Requires at least two labels after `*`                           | Permit one or more suffix labels; do not infer a public-suffix database                      |
| Trailing dot                                                                                                                | Reference may have a final dot; presented names cannot           | Reject final dots on both sides                                                              |
| [IP matcher](https://github.com/rustls/webpki/blob/3d0adc46704c1d985a9fefe2034650488d96d49e/src/subject_name/ip_address.rs) | Exact 4/16-byte equality; other lengths return false in matcher  | Same equality, but invalid length is a typed malformed-certificate failure                   |
| Name constraints                                                                                                            | Separate constraint functions                                    | Separate JUL-168 contract; never call constraint matching here                               |

These differences require explicit fixture expectations, not blindly copying the library's tests.
The versioned [fixture matrix](fixtures.md) is the implementation acceptance oracle for local
policy choices.

## Risks / Trade-offs

- An adapter could feed resolved names or omit SAN entries → require original-origin provenance
  and complete leaf data; integration acceptance must check both independently of this matcher.
- Strict malformed-name and trailing-dot policy rejects some otherwise interoperable peers →
  expose typed reasons and document it; do not weaken the policy silently.
- A public value can be manually forged → validate at both public operation boundaries; success
  certifies matching only, never trust or provenance of caller-provided data.
- Default budgets can reject large legitimate certificates → callers choose explicit limits;
  checked arithmetic and allocation-free scanning remain mandatory.
- Syntax-only A-label handling does not prove IDNA validity → require prepared inputs and avoid
  advertising Unicode/IDNA support.

## Implementation Plan

The runtime work is tracked in tasks.md section 3 and [implementation.md](implementation.md).
The pure matcher and SAN adapter, documentation and consolidated acceptance fixtures are included.
URI/IP text conversion, certificate envelope decoding, path policy and TLS composition stay with their owners.

## Runtime SAN adapter contract

The authorized JUL-184 implementation adds `silk.certificate_identities.CertificateSan`, separate
from the pure matcher. Its exact operations are:

```silk
pub fn decode<'a>(certificate: &'a Certificate, storage: &mut [PresentedIdentity<'a>], limits: SanDecodeLimits) -> Result<SanDecodeSummary, SanDecodeError>
pub fn decodeValue<'a>(value: &'a [u8], storage: &mut [PresentedIdentity<'a>], limits: SanDecodeLimits) -> Result<SanDecodeSummary, SanDecodeError>
```

`SanDecodeSummary` owns `present: bool` and `count: usize`. A successful present result authorizes
using exactly `storage[0..count]`; absent returns false/zero. Payloads borrow the input certificate
or DER bytes. Storage is initialized caller-owned memory and cannot be used concurrently with its
mutable borrow. A failure can leave a partial prefix: callers must discard that prefix and stop,
never construct a successful `CertificateIdentities` value from it. No allocation or service is
required. A helper with named lifetime `'a` can construct `CertificateIdentities<'a>.Decoded`
from `&'a [PresentedIdentity<'a>]` for verification; this makes both lifetime relationships explicit.

`SanDecodeLimits` has inclusive `extensions`, `inputBytes`, `identities`, `nodes`, and `depth`
fields. Defaults are 256 extensions, 1048576 input bytes, 256 identities, 4096 traversed nodes,
and depth 32. A fixed 32-frame stack imposes a hard depth ceiling independent of caller limits.
For certificate input, count covers every extension and input bytes sum every OID and value length.
For direct DER input, input bytes is the entire supplied value length. All sums use remaining-budget
subtraction before addition. Caller storage is an additional descriptor-count bound.

The certificate operation checks extension count, then complete input accounting and duplicate SAN
selection, before parsing. A duplicate SAN is fatal even if the first is valid. DER decoding consumes
one complete SEQUENCE with no trailing bytes, preserves all GeneralName alternatives in order,
and checks framing, primitive encodings and the known GeneralName wrappers under traversal budgets.
Unsupported alternatives keep their complete alternative content octets. DNS LDH/wildcard rules
and IP lengths are left to the matcher. OID-selected open-value semantics and X.400 attribute
semantics are outside this adapter; no successful identity claim depends on them.

`SanDecodeError` owns `kind: SanDecodeClass`, `reason: SanDecodeReason`, and `offset: usize`.
Classes distinguish `Malformed` from `ResourceLimit`. Reasons distinguish invalid DER, duplicate
SAN, empty SAN, extension count, input bytes, identity count, caller storage, node count and depth.
Offsets address the GeneralNames DER input; extension-list errors use zero. These failures remain
separate from both matcher policy and path trust. If composing through the explicit malformed
matcher input, map duplicate/empty to `DuplicateSanExtension`/`EmptySanExtension` and other
malformed encoding to `InvalidDer`; resource failures must stop before the matcher.
