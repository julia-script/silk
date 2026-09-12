# https-service-identity Specification

## Purpose

Define the bounded standard-library contract for matching an HTTPS origin's reference identity
against a leaf certificate's DNS-ID and IP-ID identities, independently of decoding and trust.
This delta governs the authorized runtime follow-through for JUL-183 and JUL-184 after the JUL-169 design handoff.

## Requirements

### Requirement: HTTPS reference identity comes only from the original origin host

The caller SHALL classify the original HTTPS origin host using RFC 3986 first-match IPv4 rules
and IPv6 literal syntax before DNS resolution. Literal addresses SHALL produce IP-ID references
with their decoded four/sixteen network-order octets; other admitted hosts SHALL produce DNS-ID
references. The caller MUST NOT derive the reference from certificate names, resolved addresses,
CNAME/DNAME, SVCB/HTTPS alternative targets, reverse DNS or search-suffix expansion. The matcher
MUST NOT include URI scheme, userinfo, port, path, query or fragment in name comparison, and MUST
NOT use URI-ID, SRV-ID, CN or other subject RDNs as HTTPS identities. The caller SHALL check the
HTTPS scheme separately and retain port-based origin isolation.

#### Scenario: DNS resolution does not redefine the reference

- **WHEN** `www.example` resolves through `edge.example` to `192.0.2.1`
- **THEN** only the original `www.example` DNS reference is matched; neither the alias nor the address authorizes the origin

#### Scenario: IP origin cannot match textual DNS SAN

- **WHEN** an HTTPS origin host is `192.0.2.1` and the only SAN is dNSName `192.0.2.1`
- **THEN** the reference is IP-ID and the SAN cannot produce a match (this profile rejects its numeric final DNS label)

### Requirement: Public inputs and results have exact ownership and typed failures

The `silk.https_identity` module SHALL expose the exact types and operation headers in
design.md Decision 1: `HttpsIdentity.reference` consumes `OriginHost<'name>` and returns
`Result<ReferenceIdentity<'name>, IdentityError>`; `HttpsIdentity.verify` borrows the reference
and `CertificateIdentities<'cert>`, consumes scalar `IdentityLimits`, and returns
`Result<IdentityMatch, IdentityError>`. DNS and decoded SAN bytes SHALL remain shared caller-owned
borrows; reference IP arrays and result/error metadata SHALL be owned values. Both operations
SHALL be pure and allocation-free, with no provider or Effect requirement. `verify` SHALL
revalidate public DNS reference values. The input model SHALL be usable with manually supplied
bytes without completed URI, X.509 or TLS implementations.

`IdentityError` SHALL distinguish malformed reference, malformed certificate structure, malformed
certificate DNS name, malformed certificate identity, unsupported reference form, limit exhaustion
and no match. These SHALL NOT be mapped to trust/path errors. Indexed failures SHALL report the
first invalid SAN's original zero-based position. Success SHALL report the first matching SAN's
original index and SHALL retain no borrow.

#### Scenario: Caller-provided data works without a decoder

- **WHEN** a caller supplies a DNS reference and a borrowed array containing decoded dNSName bytes
- **THEN** verification needs no certificate handle, allocator, filesystem, network, trust provider or URI object

#### Scenario: Direct construction cannot bypass reference validation

- **WHEN** a caller constructs a DNS reference containing NUL and a certificate has a matching prefix
- **THEN** verification returns `MalformedReference.Nul` before inspecting certificate input

### Requirement: DNS admission is a bounded ASCII profile

DNS references SHALL contain 1..253 octets, with dot-separated labels of 1..63 ASCII letters,
digits or hyphens; labels SHALL start and end in a letter or digit. The final label SHALL NOT be
all decimal digits. Empty names/labels, oversized names/labels, NUL, non-ASCII, underscores,
reference wildcards, edge hyphens and trailing dots SHALL be rejected without trimming, decoding
or normalization. A single label SHALL denote an absolute name without a search suffix.

ASCII A-label spellings SHALL be compared directly after this syntax validation. Unicode-to-IDNA,
Punycode validity, Unicode normalization and IDNA contextual validation SHALL remain outside this
slice; callers SHALL supply prepared A-labels for internationalized names. DNS error precedence
SHALL follow design.md Decision 3 so inputs with multiple defects have stable typed results.

#### Scenario: Exact A-label comparison

- **WHEN** `XN--BCHER-KVA.example` is compared with SAN `xn--bcher-kva.EXAMPLE`
- **THEN** the names match using ASCII case folding without Unicode conversion

#### Scenario: Malformed DNS input

- **WHEN** a reference is empty, contains an empty or 64-octet label, contains NUL or non-ASCII, ends in a dot, or exceeds 253 octets
- **THEN** reference construction fails with the corresponding typed malformed-reference reason

### Requirement: DNS wildcards match one complete leftmost label

DNS equality SHALL compare every label case-insensitively using ASCII A..Z folding only. A
presented wildcard SHALL occur exactly once and be the entire leftmost label; it SHALL match
exactly one nonempty reference label. After non-ASCII checks, a presented identifier violating
the single/whole-leftmost wildcard rules SHALL be ignored while other SANs remain eligible.
Other DNS syntax defects SHALL be malformed-certificate failures. A sole `*` SHALL fail with
`MissingWildcardSuffix`; a valid wildcard SHALL have one or more ordinary suffix labels.
The matcher SHALL NOT perform public-suffix lookup, partial-label wildcard matching, recursive
wildcard expansion, CN fallback or canonical-name substitution.

#### Scenario: One label versus several labels

- **WHEN** the SAN is `*.example` and references are `a.example`, `example` and `a.b.example`
- **THEN** only `a.example` matches

#### Scenario: Invalid wildcard is skipped without discarding valid SANs

- **WHEN** SANs include `w*.example`, `*.*.example` or `a.*.example` alongside valid `www.example`
- **THEN** those invalid wildcard identifiers are ignored and `www.example` can match in either order

### Requirement: Literal IP identity uses binary equality and explicit upstream normalization

IP references SHALL match only iPAddress SANs with equal-length, equal-value octets. Four-byte
IPv4 SHALL NOT match sixteen-byte mapped IPv6. SAN address lengths other than four or sixteen
SHALL fail as `MalformedCertificateIdentity.InvalidIpLength`, including constraint encodings
of eight/thirty-two bytes. No subnet, mask, textual DNS or URI comparison SHALL substitute for
binary equality.

The origin adapter SHALL validate and remove IPv6 URI brackets before binary conversion, exclude
the port, reject malformed IP literals, and return `Unsupported` for IPvFuture and zone
identifiers. This module SHALL NOT parse textual IP or URI input. First-slice HTTPS admission
SHALL reject percent-encoded origin hosts and trailing dots consistently for routing and identity;
generic URI syntax SHALL remain independently broader. Passing brackets, ports, escapes or URI
delimiters as DNS input SHALL fail rather than be normalized here.

#### Scenario: Equivalent IPv6 spellings

- **WHEN** the caller converts `[2001:db8::1]` and `[2001:0DB8:0:0:0:0:0:1]` to network-order octets
- **THEN** either reference matches the same sixteen-byte iPAddress SAN

#### Scenario: Unsupported origin form

- **WHEN** the caller supplies `OriginHost.Unsupported` for IPvFuture, a zone identifier, URI-ID or SRV-ID
- **THEN** construction returns the corresponding `Unsupported` result and never attempts DNS matching

### Requirement: Complete SAN validation precedes matching

Certificate input SHALL distinguish absent SAN, a complete decoded SAN list and structural
failure. A present empty SAN extension, malformed DER/GeneralName or duplicate SAN extension
SHALL fail structurally. Production adapters SHALL preserve every SAN in original order and
report structural errors before offering a decoded list; they MUST NOT prefilter invalid or
unsupported entries.

The delivered certificate envelope decoder's raw extension views SHALL NOT be treated as decoded
SAN identities. A separate identity-consumer adapter SHALL select subjectAltName OID `2.5.29.17`,
detect duplicate SAN extensions, decode complete GeneralNames under its own finite parsing/storage
budgets, and preserve the certificate owner and descriptor lifetimes. The adapter SHALL remain separate from the pure matcher and SHALL NOT introduce a decoder dependency in that matcher.

After reference validation, verification SHALL apply structural, count-limit, byte-limit, and
ordered per-entry validation in that precedence, before any success. It SHALL validate both DNS
and IP SANs irrespective of reference kind. Non-ASCII DNS octets SHALL fail before wildcard
checks because they invalidate IA5String. NUL SHALL be checked after invalid-wildcard skipping
because it is malformed DNS syntax, not invalid IA5 encoding. Ignorable wildcard entries SHALL
still consume budgets. Invalid ordinary DNS names or
IP lengths anywhere SHALL defeat a matching SAN in either order. Valid unsupported GeneralName
forms SHALL be ignored for matching and counted; invalid/misclassified `Other.tag` SHALL fail as
`WrongGeneralNameTag`. Absent SAN/CN-only, unsupported-only and valid mismatching lists SHALL
return `NoMatch`. `Decoded` with zero entries SHALL be `EmptySanExtension`.

#### Scenario: Malformed name cannot be hidden after a match

- **WHEN** a matching `www.example` DNS SAN and malformed `bad_name.example` are in either order
- **THEN** verification fails with the malformed SAN's index instead of returning success

#### Scenario: Structural failure is not an invalid wildcard

- **WHEN** a decoder reports invalid GeneralName structure in a certificate also containing a matching or wildcard SAN
- **THEN** verification returns the structural error and cannot treat it as an ignorable wildcard

#### Scenario: CN-only and unsupported forms

- **WHEN** a certificate has only a matching CN, or only valid URI-ID/SRV-ID SANs
- **THEN** verification returns `NoMatch` and does not fall back to those names

#### Scenario: Certificate envelope decoding is not SAN decoding

- **WHEN** certificate decoding succeeds while preserving duplicate SAN OIDs or malformed opaque SAN contents
- **THEN** the separate SAN adapter reports the appropriate structural failure rather than treating envelope success as a complete decoded identity list

### Requirement: SAN count and bytes bound matching work

The standard limits SHALL be 256 entries and 65536 payload octets. Caller-supplied nonnegative
budgets, including zero, SHALL be honored without an unlimited sentinel. Every entry and each
occurrence of its payload SHALL count, including unsupported, ignored and aliased entries.
Count overflow SHALL fail before payload inspection; byte-sum overflow or exceeding the byte
budget SHALL produce `LimitExceeded.SanBytes`. Limits SHALL be checked before content scans.
Matching SHALL require O(R + N + B) work and O(1) auxiliary storage, with reference length R,
SAN count N and summed bytes B; it SHALL NOT allocate, recurse or backtrack. Certificate DER
decoding bounds SHALL remain a separate upstream obligation.

#### Scenario: Hidden oversized input after a valid name

- **WHEN** a matching first SAN is followed by enough supported or unsupported payload to exceed either budget
- **THEN** verification returns the appropriate `LimitExceeded` failure without returning early success

#### Scenario: Boundary budgets

- **WHEN** a complete otherwise valid SAN input has exactly the configured count and byte size
- **THEN** it is admitted, while a one-entry or one-byte excess fails; absent SAN under zero budgets returns `NoMatch`

### Requirement: Identity matching stays separate from trust and has pinned fixtures

Implementation SHALL use the versioned matrix in fixtures.md, pinned to the cited RFC editions
and webpki commit `3d0adc46704c1d985a9fefe2034650488d96d49e`, preserving explicit local policy
differences. It SHALL cover exact and wildcard DNS, excess labels, A-labels, IPv4/IPv6, malformed
references/SANs, mixed-entry ordering, CN-only and limits. RFC 5280 name constraints SHALL NOT
be interpreted by this matcher. Integration SHALL require identity, trust and TLS authentication
for the same leaf; identity success alone SHALL NOT be treated as a secure connection.

#### Scenario: Correct name on an untrusted leaf

- **WHEN** a leaf's DNS SAN matches the origin but its path is untrusted or violates name constraints
- **THEN** this matcher can return identity success while the separate trust/path check rejects the connection

#### Scenario: Runtime completion

- **WHEN** the authorized runtime follow-through is handed off
- **THEN** evidence includes executing matcher and SAN adapter cases, while trust, URI/IP conversion and TLS remain separate

### Requirement: SAN adaptation uses explicit bounded caller storage

`CertificateSan.decode` and `decodeValue` SHALL implement the exact signatures and ownership in
design.md's runtime SAN adapter contract. They SHALL allocate no memory and SHALL return an owned
presence/count summary or typed structural/resource error. Callers MUST NOT consume a partial
prefix after failure. The adapter SHALL preserve all GeneralName alternatives in source order,
check complete DER framing and known primitive encodings, and use independent byte, extension,
identity, node and depth budgets plus caller storage capacity. OID-selected open-value semantics
and X.400 attribute semantics SHALL remain outside identity verification.

#### Scenario: Caller storage is insufficient

- **WHEN** a valid GeneralNames list contains more entries than caller storage can hold
- **THEN** decoding fails with the typed storage resource limit and its partial prefix cannot authorize a match

#### Scenario: An unsupported name contains malformed primitive DER

- **WHEN** a matching DNS SAN is followed by an otherName containing a noncanonical universal primitive
- **THEN** adaptation fails structurally before any successful identity result is available

#### Scenario: Independent decode limits

- **WHEN** extension/input/identity/node/depth limits are exceeded, including nested unsupported forms
- **THEN** adaptation returns its corresponding typed resource failure without unbounded allocation or traversal
