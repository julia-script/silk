## Purpose

Provide lossless generic URI handling for Silk programs before scheme, transport, and identity policy is applied.

## ADDED Requirements

### Requirement: Owned URI values preserve lexical components

The standard library SHALL provide distinct `Uri` and `UriReference` values in ordinary Silk.
`Uri` SHALL require a scheme and accept the RFC 3986 `URI` production, including an optional fragment.
`UriReference` SHALL also accept `relative-ref`. Each value SHALL own one original serialization
and store validated byte ranges. Accessors SHALL borrow from that owner without allocation or
self-references. Ranges SHALL remain within the immutable backing text and exclude component
delimiters, except that bracketed hosts SHALL retain their brackets. Optional authority, userinfo,
host, port, query, and fragment SHALL distinguish absent from present-empty. Path SHALL always be
present, including the empty path. Formatting SHALL return the original serialization exactly.

#### Scenario: Preserve empty delimiters and escape case

- **WHEN** `x://@:/%2e?` is parsed and formatted
- **THEN** formatting returns that exact text, authority and query are present, userinfo, host, and port are empty, and fragment is absent

#### Scenario: Own the source and reborrow components

- **WHEN** a successful parse outlives the caller's input storage
- **THEN** formatting and accessors remain valid while the parsed owner lives, and borrowed views cannot outlive that owner

### Requirement: Parsing implements generic RFC 3986 syntax

Parsing SHALL validate schemes, every path form, query, fragment, authority, and percent escapes
under RFC 3986. Userinfo SHALL remain one lexical component. Port SHALL accept zero or more digits
without a numeric range limit. Host classification SHALL prefer IPv4 when the entire host matches
the RFC decimal-octet production, then reg-name. Bracketed IPv6 and IPvFuture SHALL be validated.
Zone identifiers, raw non-ASCII, and invalid delimiter placement SHALL be rejected. Failures SHALL
include a typed reason, component, and zero-based byte offset. Invalid input SHALL not allocate
owned output. Allocation failure SHALL remain separate from syntax failure.

#### Scenario: Parse rootless schemes

- **WHEN** `mailto:person@example.com` or `urn:example:thing` is parsed
- **THEN** it succeeds with a scheme and rootless path and no authority

#### Scenario: Classify lexical hosts

- **WHEN** `//127.0.0.1:999999`, `//01.2.3.4`, `//[::ffff:192.0.2.1]`, and `//[vF.a:b]` are parsed
- **THEN** their hosts are respectively IPv4, reg-name, IPv6, and IPvFuture, and the large port remains lexical text

#### Scenario: Reject malformed syntax precisely

- **WHEN** a component contains `%`, `%0G`, unmatched brackets, an invalid IP literal, or an illegal ASCII byte
- **THEN** parsing returns a typed failure at the offending byte or at the end where required syntax is missing

### Requirement: Percent coding keeps byte data separate from text

Percent decoding SHALL return arbitrary bytes and reject malformed escapes with their byte offsets.
It SHALL leave plus signs unchanged. UTF-8 validation SHALL be a separate explicit String operation.
Encoding SHALL take raw component bytes and encode percent signs as data. It SHALL provide
component contexts for userinfo, reg-name, path segment, first relative path segment, query, and
fragment, retaining only characters allowed in that context and uppercase newly generated escapes.
It SHALL NOT decode or normalize existing serialization implicitly. Query encoding SHALL implement
generic query syntax, not form fields. Encoding segments SHALL escape slash; encoding first relative
segments SHALL also escape colon so data cannot become a scheme.

#### Scenario: Decode non-text data

- **WHEN** `%FF+%00` is decoded
- **THEN** it returns bytes FF, 2B, 00 and explicit UTF-8 validation rejects them

#### Scenario: Encode component delimiters as data

- **WHEN** raw bytes `a/b%2f` are encoded as a path segment
- **THEN** the result is `a%2Fb%252f`

### Requirement: Resolution follows the strict reference algorithm

Resolution SHALL accept a parsed scheme-bearing base and parsed reference and implement RFC 3986
section 5.2 in strict mode. It SHALL preserve the spelling of copied components and remove only
literal dot segments from the selected or merged path. A successful result SHALL be a scheme-bearing URI. Recomposition SHALL concatenate components
according to section 5.3 and revalidate the resulting serialization. If dot removal exposes `//`
without an original authority, those bytes SHALL be interpreted under the resulting URI grammar;
invalid recomposed syntax SHALL return a typed parse failure even when the inputs were valid.
Normal and abnormal examples in section 5.4 SHALL all match, including strict `http:g` handling.

#### Scenario: Resolve all standard examples

- **WHEN** each section 5.4 reference is resolved against `http://a/b/c/d;p?q`
- **THEN** the result equals the corresponding strict RFC example

#### Scenario: Keep encoded dots and query data

- **WHEN** `%2e/%2E%2e/g?y/../x` is resolved against `http://a/b/c/d;p?q`
- **THEN** the result is `http://a/b/c/%2e/%2E%2e/g?y/../x`

#### Scenario: Revalidate authority syntax exposed by dot removal

- **WHEN** `/a/..//g` and `/a/..//g:h` are resolved against `x:a`
- **THEN** the first succeeds as `x://g` with authority `g`, and the second fails with invalid port syntax at byte 6 of the recomposed serialization

### Requirement: URI handling ships without transport policy

The manifest, generated source table, generated API documentation, and prescriptive reference
SHALL expose the actors and their contracts. Implementation SHALL use ordinary source and existing
allocation primitives. It SHALL NOT implement WHATWG URL, IRI normalization, IDNA, DNS resolution,
TLS identity, file semantics, username/password interpretation, service lookup, or canonicalization.

#### Scenario: Use syntax without network services

- **WHEN** an application imports the URI actors
- **THEN** it can parse, format, encode, decode, and resolve with only an allocator for owned output
