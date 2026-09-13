## Purpose

Provide one bounded, byte-preserving HTTP/1.x value model that parsers, clients, and servers can
share without coupling syntax and metadata to body framing, transport, or connection state.

## ADDED Requirements

### Requirement: HTTP versions, methods, and statuses preserve their protocol meaning

The `silk.http` actor SHALL expose `Version`, `Method`, and `Status` values in ordinary Silk.
`Version.parse` and `Version.format` SHALL recognize and emit exactly HTTP/1.0 and HTTP/1.1.
`Method.parse`, `Method.format`, and `Method.equals` SHALL borrow and
preserve every nonempty case-sensitive `tchar` token within an explicit byte bound; known methods
SHALL be conveniences rather than a closed admission set. `Status` SHALL admit every code from 100
through 599 through `Status.fromCode`, preserve unregistered codes, and expose its numeric code and
class through `Status.code` and `Status.class`. Status
semantics SHALL NOT depend on a reason phrase or a universal request-body predicate.

#### Scenario: Preserve supported and extension protocol values

- **WHEN** both supported versions, extension methods `PURGE` and `purge`, and status 299 are constructed and formatted
- **THEN** both versions round-trip exactly, the extension method spellings remain distinct, and status 299 remains a successful-class code

#### Scenario: Reject invalid scalar values at their boundaries

- **WHEN** an unsupported version, an empty or malformed method token, a method beyond its byte bound, or status 99 or 600 is constructed
- **THEN** construction returns the corresponding typed value or limit failure without allocation

### Requirement: Headers preserve validated names and raw field octets

`silk.http.Header.make` SHALL borrow its original nonempty `tchar` name and exact field value bytes.
Name lookup
SHALL be ASCII case-insensitive without changing stored spelling. Field values SHALL admit empty
values, interior HTAB and SP, visible ASCII, and obs-text bytes. The semantic constructor SHALL
reject leading or trailing optional whitespace and every forbidden control, including CR, LF, NUL,
DEL, and controls other than HTAB. Invalid unchecked headers SHALL not be constructible through the
public API.

#### Scenario: Preserve valid byte-oriented fields

- **WHEN** headers contain an empty value, an interior tab, obs-text, and differently cased `Set-Cookie` names
- **THEN** construction succeeds and iteration exposes the original names and exact value octets unchanged

#### Scenario: Reject injection and ambiguous semantic whitespace

- **WHEN** a name is empty or not a token, or a value contains CRLF, NUL, DEL, another forbidden control, or leading or trailing OWS
- **THEN** construction returns a typed failure identifying the header component and offending byte

### Requirement: Header collections retain order and duplicates without implicit combination

`silk.http_headers` SHALL expose `Headers`, `OwnedHeaders`, `HeaderIterator`, and `Limits`.
`Headers.make` SHALL provide an ordered borrowed view over validated entries or an owned backing
store. Construction SHALL validate field count and aggregate name-plus-value bytes using checked
arithmetic. Iteration SHALL preserve insertion order, original name case, and duplicates.
`getAll`, `getFirst`, and `getUnique` SHALL match names ASCII case-insensitively; `getUnique` SHALL
fail when more than one field matches. No generic operation SHALL implicitly comma-concatenate
field values.

#### Scenario: Look up repeated Set-Cookie fields without loss

- **WHEN** `Set-Cookie: a=1` precedes `set-cookie: b=2` and lookup uses `SET-COOKIE`
- **THEN** iteration and `getAll` return both fields in that order, `getFirst` returns `a=1`, and `getUnique` reports a duplicate

#### Scenario: Reject collection limits and arithmetic overflow

- **WHEN** field count or aggregate name-plus-value bytes reaches one past its explicit bound, or aggregate size cannot be represented
- **THEN** construction fails with the specific limit or size-overflow reason before allocating or exposing a partial view

### Requirement: Header formatting is sized, field-only, and mutation-atomic on capacity failure

`Headers.formatInto` SHALL first compute the complete checked size, then emit each
field as `name: value\r\n`. An empty value SHALL still emit one SP after the colon. Insufficient
caller storage SHALL return required and available sizes without modifying any output byte. Header
formatting SHALL NOT emit a start line or the final empty-line delimiter.

#### Scenario: Format ordered fields

- **WHEN** a sufficient output buffer receives two headers including an empty value
- **THEN** the exact ordered `name: value\r\n` bytes are written and the returned size excludes any start line or final delimiter

#### Scenario: Preserve too-small output

- **WHEN** the caller buffer is one byte smaller than the checked required size
- **THEN** formatting returns `OutputTooSmall` with required and available sizes and every original output byte remains unchanged

### Requirement: Owned headers and heads have explicit affine lifetime boundaries

`Headers.copy` SHALL allocate an `OwnedHeaders`; owned request and response head copy operations
SHALL allocate `OwnedRequestHead` and `OwnedResponseHead`. These owners SHALL copy borrowed payloads into
independent owned storage and expose borrowed views tied to the owner. Owned headers SHALL retain
payload bytes plus index information rather than self-references, and each iterated header SHALL be
resolved on demand against the currently borrowed owner. Owned heads SHALL cover method, target,
fields, and optional response reason phrase. Dropping an owner SHALL release all acquired storage;
neither an owned view nor an iterator result SHALL outlive its owner or borrowed input.

#### Scenario: Copy survives source release

- **WHEN** a valid borrowed collection or head is copied and its original backing storage is released
- **THEN** a view of the owner still exposes the exact method, target, fields, and reason bytes

#### Scenario: Ownership analysis rejects escaping views

- **WHEN** a program attempts to return an owner-derived head view, header iterator, or yielded header after the owner is dropped
- **THEN** ownership analysis rejects the program before execution

#### Scenario: Allocation failure releases partial owned storage

- **WHEN** a deterministic allocator fails at each acquisition boundary while copying a multi-field value
- **THEN** the operation reports only `OutOfMemoryError`, releases every prior acquisition exactly once, and exposes no partial owner

### Requirement: Limits are explicit, finite, and enforced before excess allocation

Every applicable operation SHALL receive finite limits for method bytes, target bytes, name bytes,
value bytes, field count, aggregate field bytes, and owned bytes. Zero SHALL be a real bound; no
unlimited sentinel or hidden default SHALL exist. Owned-byte accounting SHALL include payload
capacities and collection/index allocations while excluding allocator bookkeeping and caller
buffers. Semantic invalidity SHALL be rejected before allocation, and an operation SHALL reject a
limit boundary before performing the acquisition that would exceed it.

#### Scenario: Exercise every exact limit boundary

- **WHEN** valid input is tested at each exact limit and again one unit beyond it
- **THEN** the exact-bound case succeeds, the over-bound case reports allowed and attempted counts for the named limit, and no excess acquisition occurs

#### Scenario: Keep allocation failure distinct from semantic failure

- **WHEN** invalid input is supplied through an allocator configured to fail immediately
- **THEN** the semantic `ValueError` is returned without invoking the allocator

### Requirement: Request targets preserve all four HTTP forms and enforce method-specific syntax

`silk.http_target` SHALL expose `RequestTarget` and `HttpAuthority`. `RequestTarget.parse` SHALL
represent origin, absolute, authority, and asterisk forms while preserving
accepted spelling, percent escapes, and case. Raw parsing SHALL reject fragments, whitespace,
controls, non-ASCII bytes, and userinfo in HTTP authorities. CONNECT SHALL require authority-form
with a nonempty host and numeric port from 1 through 65535; OPTIONS SHALL be allowed to use `*` and
other methods SHALL not. Bracketed IPv6 SHALL remain bracketed. Absolute-form SHALL retain generic
URI schemes; transport-level scheme admission SHALL remain outside this capability.

#### Scenario: Parse every request-target form

- **WHEN** table cases cover origin-form, a generic absolute URI, CONNECT with a bracketed IPv6 host and port, and `OPTIONS *`, including an extension method
- **THEN** each result has the selected form and formats with its accepted lexical spelling unchanged

#### Scenario: Reject form and destination mismatches

- **WHEN** CONNECT lacks a nonempty host or valid port, a non-OPTIONS method uses `*`, or any raw target contains a fragment, userinfo, whitespace, control, or non-ASCII byte
- **THEN** parsing returns the precise target, authority, port, or method-target mismatch failure

### Requirement: URI conversion and HTTP authority selection are explicit

`RequestTarget.fromUri` SHALL convert a generic `Uri` to origin-form or absolute-form. Origin-form
SHALL emit encoded path and optional query, map an empty
path to `/`, preserve a present empty query, and exclude the fragment. Converting to absolute-form
SHALL retain the URI serialization except for the fragment. Conversion SHALL reject userinfo and
insufficient output without resolving DNS or connecting. `HttpAuthority` SHALL distinguish an empty
host, reg-name, bracketed IP literal, and optional numerically checked port; zone identifiers, IDNA,
and network admissibility SHALL remain boundary policies.

#### Scenario: Convert an empty-path URI without sending its fragment

- **WHEN** `http://example.com?x=#section` is converted to origin-form and outgoing Host
- **THEN** the target is `/?x=`, Host is `example.com`, and no fragment byte is emitted

#### Scenario: Preserve the raw-parse versus URI-conversion distinction

- **WHEN** a raw target contains a fragment and a parsed generic URI containing the same fragment is converted
- **THEN** raw target parsing rejects the fragment while URI conversion deliberately omits it

#### Scenario: Reject HTTP userinfo instead of converting it

- **WHEN** URI-to-target or outgoing-Host conversion receives an authority with userinfo
- **THEN** conversion returns `InvalidAuthority` and emits no credentials or partial output

### Requirement: HTTP/1.1 Host validation and effective authority do not invent equality policy

Request-head validation SHALL reject a missing HTTP/1.1 Host field, repeated Host fields, and a
Host value that is not a valid HTTP authority. HTTP/1.0 SHALL not require Host. For absolute-form
and CONNECT, effective-authority selection SHALL use the request target's authority even when a
valid Host uses different spelling or value. For origin-form and asterisk-form it SHALL use Host,
retaining an explicit empty authority for later routing policy. Outgoing Host derivation SHALL use
the URI authority without applying DNS, IDNA, default-port, or override-equality policy.

#### Scenario: Validate Host cardinality and syntax

- **WHEN** HTTP/1.1 requests respectively omit Host, repeat Host, or contain an invalid Host authority
- **THEN** validation reports `MissingHost`, `DuplicateHost`, or `InvalidAuthority` with the matching field index where applicable

#### Scenario: Prefer target authority without rejecting a differing Host

- **WHEN** an absolute-form or CONNECT target has one valid authority and its single valid Host field has another
- **THEN** validation succeeds and effective-authority selection returns the target authority

### Requirement: Response heads validate optional reason bytes independently of status

`ResponseHead` SHALL contain only Version, Status, an optional borrowed reason phrase, and Headers.
Reason bytes SHALL be bounded by `maxValueBytes`; empty, HTAB, SP, visible ASCII, and obs-text SHALL
be admitted, while CR, LF, NUL, DEL, and other controls SHALL fail. A reason phrase SHALL never
determine or replace status semantics. `RequestHead` SHALL contain only Version, Method,
RequestTarget, and Headers. Neither head SHALL contain a body, framing decision, connection, or
exchange reservation.

#### Scenario: Validate response reason bytes independently

- **WHEN** an unregistered valid status is paired with absent, empty, visible, tabbed, or obs-text reason bytes
- **THEN** each valid head retains the same status and exact optional reason bytes

#### Scenario: Reject unsafe or over-limit reason bytes

- **WHEN** a reason contains a forbidden control or exceeds `maxValueBytes`
- **THEN** head construction returns the corresponding byte or limit failure without changing status semantics

### Requirement: Metadata iterators retain unknown tokens and locate grammar errors

Headers SHALL expose bounded borrowed iterators for Connection tokens, Content-Encoding codings,
and Transfer-Encoding codings. Connection and Content-Encoding SHALL accept token-list grammar
without parameters. Transfer-Encoding SHALL preserve and validate its separate transfer-parameter
grammar. Unknown valid tokens SHALL be yielded unchanged and known-token recognition SHALL be
ASCII case-insensitive. Grammar failures SHALL identify the source field index and byte offset.
These iterators SHALL not compute content length, transfer precedence, decoder support, or
connection reuse eligibility.

#### Scenario: Retain known and extension metadata

- **WHEN** repeated metadata fields contain mixed-case known tokens and syntactically valid extension tokens or transfer parameters
- **THEN** iteration preserves source order and spelling while optional known-token recognition remains case-insensitive

#### Scenario: Report parameter grammar at its source

- **WHEN** Connection or Content-Encoding contains a parameter, or Transfer-Encoding contains a malformed parameter
- **THEN** iteration returns a `ValueError` carrying the originating field index and value-byte offset

### Requirement: HTTP values expose a closed typed failure contract on portable targets

Pure HTTP value operations SHALL fail only with `ValueError`, whose `reason`, `component`, optional
field index, and byte offset identify semantic failures. Reasons SHALL cover invalid tokens and
bytes, unsupported versions, invalid statuses, targets, authorities and ports, method/target
mismatch, Host cardinality, named limit excess with allowed and attempted counts, size overflow,
and insufficient output with required and available sizes. The public reason variants SHALL be
`InvalidToken`, `InvalidValueByte`, `UnsupportedVersion`, `InvalidStatus`, `InvalidTarget`,
`InvalidAuthority`, `InvalidPort`, `MethodTargetMismatch`, `MissingHost`, `DuplicateHost`,
`LimitExceeded`, `SizeOverflow`, and `OutputTooSmall`; the limit and output reasons SHALL retain
their named counts. Owned-copy allocation failure SHALL use
the distinct existing `OutOfMemoryError` effect. The capability SHALL be available from ordinary
Silk source on supported native and LLVM-to-Wasm paths without an OS/link prerequisite.

#### Scenario: Keep pure failure channels closed

- **WHEN** every malformed or bounded input family is exercised
- **THEN** callers can exhaustively distinguish the documented `ValueError` reasons and never receive an unknown, exception, network, or timeout failure

#### Scenario: Use the same values across targets

- **WHEN** a shared memory-only acceptance program runs through supported native and LLVM-to-Wasm execution
- **THEN** it observes the same validated value, ordering, lookup, target, and formatting behavior, with an allocator required only for explicit owned copies

### Requirement: Public documentation records ownership, limits, and deliberate strictness

The manifest, generated source inventory, generated API reference, and prescriptive standard-library
reference SHALL expose the HTTP actors and document byte ownership, lifetime boundaries, all
required limits, target availability, raw parsing versus URI conversion, and deliberate differences
from the pinned Zig comparison. The documentation SHALL clearly assign incremental head parsing and
complete-head serialization, body framing and trailers, content decoding, connection reuse,
transport/exchange, proxies, and client/server behavior to their dependent capabilities.

#### Scenario: Generate complete public surfaces

- **WHEN** standard-library generation and documentation validation run
- **THEN** every public HTTP actor is registered and documented with no HTTP-specific compiler recognition or undocumented compatibility path
