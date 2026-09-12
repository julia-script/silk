## Purpose

Provide lossless generic URI handling for Silk programs before scheme, transport, and identity policy is applied.

## ADDED Requirements

### Requirement: Borrowed and owned URI values preserve lexical components

The standard library SHALL provide distinct `Uri` and `UriReference` values in ordinary Silk.
`Uri` SHALL require a scheme and accept the RFC 3986 `URI` production, including an optional fragment.
`UriReference` SHALL also accept `relative-ref`. Parsing SHALL borrow input without allocation
and store validated byte ranges. Explicit owned variants SHALL support copying borrowed input and
adopting existing owned text without copying. Accessors SHALL borrow without allocation or self-references. Ranges SHALL remain within the immutable backing text and exclude component
delimiters, except that bracketed hosts SHALL retain their brackets. Optional authority, userinfo,
host, port, query, and fragment SHALL distinguish absent from present-empty. Path SHALL always be
present, including the empty path. Formatting SHALL return the original serialization exactly.

#### Scenario: Preserve empty delimiters and escape case

- **WHEN** `x://@:/%2e?` is parsed and formatted
- **THEN** formatting returns that exact text, authority and query are present, userinfo, host, and port are empty, and fragment is absent

#### Scenario: Borrow input without allocating

- **WHEN** existing text is parsed
- **THEN** parsing needs no allocator and the result cannot outlive the input

#### Scenario: Explicitly own the source

- **WHEN** a borrowed parsed value is copied or existing owned text is adopted
- **THEN** the owned result can outlive the original borrowed input, adoption does not allocate or copy, and views cannot outlive the owner

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
component contexts for userinfo, reg-name, path segment, first relative path segment, whole path,
unreserved-only, query, and fragment, retaining only characters allowed in that context and uppercase newly generated escapes.
It SHALL NOT decode or normalize existing serialization implicitly. Query encoding SHALL implement
generic query syntax, not form fields. Encoding segments SHALL escape slash; encoding first relative
segments SHALL also escape colon so data cannot become a scheme.

#### Scenario: Decode non-text data

- **WHEN** `%FF+%00` is decoded
- **THEN** it returns bytes FF, 2B, 00 and explicit UTF-8 validation rejects them

#### Scenario: Encode component delimiters as data

- **WHEN** raw bytes `a/b%2f` are encoded as a path segment
- **THEN** the result is `a%2Fb%252f`

### Requirement: Callers control percent-coding output storage

Percent coding SHALL support reusable output buffers, streaming encoding through a writer, and
strict in-place decoding. Owned conveniences SHALL calculate size and allocate backing storage
once, without zero-filling bytes that will be overwritten. A decoding fast path SHALL borrow input
without copying when there are no escapes. Malformed escapes SHALL be rejected before buffer
mutation. In-place decoding SHALL return the initialized-prefix length.

#### Scenario: Reuse output capacity

- **WHEN** encoding or decoding uses a destination with sufficient reserved capacity
- **THEN** no allocation occurs and only the produced bytes are initialized

#### Scenario: Decode in place safely

- **WHEN** `%41+%FF` is decoded in place
- **THEN** the initialized prefix is bytes 41, 2B, FF, and a malformed input leaves its buffer unchanged

### Requirement: Components support construction and selective serialization

Callers SHALL be able to construct and modify URI references from structured components, explicitly
marking raw bytes versus already percent-encoded text. Raw data SHALL be escaped for its context;
encoded text SHALL preserve spelling and be validated so delimiters cannot silently change its
component. Optional components SHALL preserve absence versus empty values. Serialization SHALL
support full output, omission of authentication, and path/query output into caller-controlled storage.
Lexical userinfo and port semantics and strict host grammar SHALL remain unchanged.

#### Scenario: Modify a path without double encoding

- **WHEN** a parsed reference's path is replaced with a raw path containing `%` and a space
- **THEN** serialization escapes that data while unchanged encoded components retain their spelling

#### Scenario: Serialize without authentication

- **WHEN** a URI with userinfo is serialized with authentication omitted
- **THEN** no userinfo or its at-sign delimiter is emitted, while host, port, path, query, and fragment are preserved

### Requirement: Resolution follows the strict reference algorithm

Resolution SHALL accept a parsed scheme-bearing base and parsed reference and implement RFC 3986
section 5.2 in strict mode. It SHALL support caller-owned reusable output storage and perform
dot removal directly in that storage without separately backed merge/normalization temporaries.
Owned convenience results SHALL adopt the completed serialization without copying it. It SHALL preserve the spelling of copied components and remove only
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
allocation primitives plus a general lifetime-preserving bounds-checked slice view. It SHALL NOT implement WHATWG URL, IRI normalization, IDNA, DNS resolution,
TLS identity, file semantics, username/password interpretation, service lookup, or canonicalization.

#### Scenario: Use syntax without network services

- **WHEN** an application imports the URI actors
- **THEN** it can parse, format, encode, decode, and resolve with only an allocator for owned output
