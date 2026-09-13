## Purpose

Define owned network address values and deterministic, finite host resolution semantics, including
the deliberately synchronous native DNS deadline and cancellation contract.

## ADDED Requirements

### Requirement: Network values own validated bounded representations

The standard library SHALL expose distinct owned IPv4, IPv6, endpoint, port, domain-host, and
classified host values. IPv4 and IPv6 SHALL own network-order `[u8; 4]` and `[u8; 16]` arrays;
IPv4 and IPv4-mapped IPv6 SHALL remain distinct. A port constructor SHALL accept exactly 0 through
65535, including zero for listener binding. An endpoint SHALL own an IP address and port.

A domain host SHALL preserve its admitted source spelling in owned storage of at most 253 bytes,
compare ASCII case-insensitively, and retain no borrow from URI or resolver input. Domain labels
SHALL contain 1 through 63 ASCII letters, digits, or hyphens, start and end with a letter or digit,
and total 1 through 253 bytes. Empty labels, trailing dots, NUL, non-ASCII, escapes, Unicode
mapping, invalid A-label syntax, edge hyphens, and an all-decimal final label SHALL be rejected.

#### Scenario: Boundary values remain owned and distinct

- **WHEN** callers construct ports 0 and 65535, an IPv4 value, its mapped-IPv6 spelling, and a
  253-byte valid domain from temporary inputs
- **THEN** both ports succeed, the address families remain distinct, and every returned value
  remains valid after the inputs are gone

#### Scenario: Domain admission rejects unsupported forms

- **WHEN** a host is empty, oversized, has a 64-byte label, trailing dot, Unicode, percent escape,
  zone identifier, IPvFuture spelling, userinfo, port delimiter, edge hyphen, or decimal final label
- **THEN** construction fails with an invalid-host or unsupported-form reason before resolution

### Requirement: Textual IP parsing and formatting are exact

IPv4 parsing SHALL accept only RFC 3986 decimal dotted quads with four components valued 0 through
255 and SHALL reject octal, hexadecimal, short, integer, signed, padded, empty, and overflowing
forms. IPv6 parsing SHALL accept RFC 4291 hexadecimal groups with at most one `::`, embedded strict
IPv4 where admitted, and exactly sixteen decoded octets; it SHALL reject zones and IPvFuture.

Canonical formatting SHALL use RFC 5952 lowercase hexadecimal, omit leading zeroes, compress the
longest run of at least two zero groups, choose the leftmost run on ties, and never compress one
zero group. Mapped addresses SHALL format as IPv6 and SHALL not become IPv4 values. Address display
SHALL use the current Writer-backed Display contract and an allocation-free `formatInto` form.

#### Scenario: Canonicalize equivalent IPv6 spellings

- **WHEN** equivalent expanded and compressed spellings of `2001:db8::1` are parsed and formatted
- **THEN** both yield identical network-order bytes and the canonical text `2001:db8::1`

#### Scenario: Select the longest leftmost zero run

- **WHEN** an IPv6 value contains tied longest runs and a separate single zero group
- **THEN** formatting compresses the leftmost longest run and writes the single group explicitly

#### Scenario: Reject non-RFC IPv4 forms

- **WHEN** input uses `127.1`, `2130706433`, `0x7f.0.0.1`, `01.2.3.4`, or a component above 255
- **THEN** IPv4 parsing fails rather than delegating to libc interpretation

### Requirement: Host classification preserves origin identity

Host classification SHALL apply RFC 3986 IP-literal and first-match IPv4 syntax before domain
validation. URI adaptation SHALL remove IPv6 brackets only after syntax classification and SHALL
not pass userinfo, port, escapes, IPvFuture, or zone identifiers to domain validation. A resolved
address, CNAME, canonical alias, or reverse lookup SHALL never replace the original domain host
used for HTTP origin identity, TLS SNI, or certificate verification.

Every admitted domain, including a single label, SHALL be absolute. Native lookup SHALL append one
root-terminating dot to the preserved unrooted source spelling; it SHALL not request search-list
expansion or publish a canonical alias.

#### Scenario: Resolve without changing TLS identity

- **WHEN** mixed-case `Example.COM` resolves to an address through an alias
- **THEN** the endpoint owns only the address and port while the original owned `Example.COM`
  remains the HTTP/TLS identity

#### Scenario: Root-terminate a single label

- **WHEN** the domain host `intranet` is sent to the native resolver
- **THEN** the libc nodename argument is exactly `intranet.` followed by NUL

### Requirement: Resolution requests and results are finite and typed

A resolution request SHALL contain an owned validated Host, checked port, family selection
`Any`/`V4`/`V6`, `maxResults` in 1 through 64, and `Option<Instant>` absolute deadline. Invalid
request fields SHALL fail before provider dispatch. A successful result SHALL own at most
`maxResults` endpoints and expose no native pointer or input borrow.

Results SHALL preserve provider order after stable exact-address deduplication and family
filtering. A further distinct admitted endpoint SHALL fail `LimitExceeded` rather than truncate;
an empty admitted result SHALL fail `NoAddress`. Expected failures SHALL distinguish invalid input,
unsupported form, timeout, unsupported deadline, no address/name not found, temporary failure,
unsupported family, system resources, limit exhaustion, invalid native result, and native failure.
A native failure SHALL retain its operation, EAI code, and errno only for `EAI_SYSTEM`.
Allocation failure SHALL remain the existing `OutOfMemoryError` instead of being erased into a
resolver error.

#### Scenario: Stable deduplication reaches the cap

- **WHEN** a provider yields duplicate A, distinct B, duplicate A, and distinct C in that order
  under `maxResults = 2`
- **THEN** duplicates do not consume capacity, A and B preserve order, and C causes
  `LimitExceeded`

#### Scenario: Distinguish no address from name failure

- **WHEN** lookup reports EAI_NONAME
- **THEN** resolution returns `NameNotFound` without claiming an observed DNS NXDOMAIN packet

### Requirement: Numeric hosts bypass name resolution with exact deadline preflight

Resolving a numeric Host SHALL invoke no Resolver provider or native lookup. With an absolute
deadline, it SHALL sample the active monotonic clock exactly once before publishing a result and
fail `Timeout` when the deadline is reached or past. Otherwise it SHALL return exactly one owned
endpoint after family filtering; an excluded numeric family SHALL return `NoAddress`.

#### Scenario: Future numeric deadline succeeds without lookup

- **WHEN** a numeric request has a future deadline and matching family selection
- **THEN** one clock sample occurs, no Resolver provider operation occurs, and one endpoint is
  returned

#### Scenario: Reached numeric deadline wins

- **WHEN** a numeric request's deadline equals the sampled monotonic instant
- **THEN** resolution returns `Timeout` before result publication and performs no lookup

### Requirement: Resolver providers own deadline and cancellation behavior

For a domain request with `Some(deadline)`, every Resolver provider SHALL either enforce that
deadline together with documented parking and cancellation ownership or return
`DeadlineUnsupported` before observable work. A capable parked provider SHALL release its Wake,
registration, and result owner on structured cancellation, and late completion SHALL neither leak
nor report false success. Cancellation SHALL remain the structured Execution outcome rather than a
resolver error.

#### Scenario: Capable provider is cancelled while parked

- **WHEN** structured cancellation destroys a deadline-capable deterministic resolution while it
  is parked
- **THEN** its registration, Wake, and result owner are released exactly once and no success is
  published

#### Scenario: Incapable provider rejects before work

- **WHEN** a domain request with a deadline reaches a provider without an in-flight deadline owner
- **THEN** it returns `DeadlineUnsupported` before query, allocation, clock sampling, or parking

### Requirement: Native domain resolution is deliberately synchronous

`NativeSystemResolver` SHALL be available only for Darwin ARM64 system libc and GNU Linux
x86-64/ARM64 GNU libc. For Domain plus `Some(deadline)`, it SHALL return
`DeadlineUnsupported` before clock sampling, request allocation, `getaddrinfo`, DNS/NSS work, or
other observable native work. For Domain plus `None`, it SHALL perform exactly one synchronous
`getaddrinfo` call. Once entered it SHALL expose no timeout or cancellation boundary and MAY block
the host scheduler thread, sibling fibers, and timers indefinitely until libc returns.

The native call SHALL receive the source-spelling domain plus `.` and NUL, decimal service digits,
and explicit `AF_UNSPEC`/`AF_INET`/`AF_INET6`, `SOCK_STREAM`, `IPPROTO_TCP`, and
`AI_NUMERICSERV` hints. This contract governs the libc argument and SHALL make no claim about libc
DNS packets, NSS implementation, elapsed time, complete-chain traversal, or process memory.

#### Scenario: Native deadline rejection has strict precedence

- **WHEN** a malformed native environment would otherwise fail a domain lookup carrying a deadline
- **THEN** `DeadlineUnsupported` is returned before any clock, allocator, or native call can expose
  the other failure

#### Scenario: Deadline-free native lookup blocks once

- **WHEN** a valid domain request without a deadline reaches the native provider
- **THEN** exactly one `getaddrinfo` call occurs and no timeout, cancellation, or scheduler-parking
  guarantee begins after entry

### Requirement: Native result traversal validates shape and frees ownership exactly once

The native provider SHALL validate every successful result node's family, socket-address pointer,
and target-specific minimum length before reading or copying it. Known IPv4/IPv6 nodes excluded by
the requested family SHALL be filtered; unknown families, null socket addresses, undersized
records, and malformed lengths SHALL fail `InvalidNativeResult`. Stable exact-address
deduplication, requested result capacity, and error precedence SHALL follow the common Resolver
contract.

Every successful non-null addrinfo chain SHALL be passed to `freeaddrinfo` exactly once after
success and every post-lookup failure, including malformed shape, capacity exhaustion, and Silk
allocation refusal. Numeric bypass, preflight deadline rejection, and failed `getaddrinfo` calls
SHALL have no chain to free. No libc pointer SHALL cross the operation boundary.

#### Scenario: Malformed later node releases the full chain

- **WHEN** a successful chain begins with one valid address and later contains an undersized
  socket address
- **THEN** resolution fails `InvalidNativeResult`, publishes no partial result, and frees the
  original chain exactly once

#### Scenario: Allocation refusal releases the chain

- **WHEN** owned result storage is refused after `getaddrinfo` returned a successful chain
- **THEN** `OutOfMemoryError` propagates and the full chain is freed exactly once

### Requirement: Portability and evidence do not overstate native guarantees

Pure values and deterministic Resolver providers SHALL compile for admitted native targets and
LLVM-to-Wasm. Native resolver declarations SHALL be absent for WebAssembly, no-libc, musl,
Windows, and unsupported architectures. Conformance SHALL use offline deterministic source
fixtures and target C stubs rather than live DNS.

Evidence SHALL cover address/domain boundaries, ownership, service replacement, numeric deadline
preflight, capable-provider cancellation cleanup, exact native arguments, per-target
addrinfo/sockaddr layout, target EAI mapping, EAI_SYSTEM errno capture, filtering versus malformed
result rejection, deduplication/capacity, and exactly-once release in debug and optimized modes.
Public documentation SHALL state that the 64-endpoint cap does not bound libc allocation,
traversal, time, scheduler blockage, or process memory.

#### Scenario: Select a portable target

- **WHEN** the pure address and deterministic Resolver contracts compile for Wasm
- **THEN** they contain no native resolver import, while importing `NativeSystemResolver` is
  rejected by source selection

#### Scenario: Conformance runs without network access

- **WHEN** the resolver acceptance suite executes offline
- **THEN** deterministic providers and target C stubs prove the contract without querying live DNS
