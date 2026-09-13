## Context

See `proposal.md` for motivation and `specs/network-address-resolution/spec.md` for the observable
contract. The current standard library has Writer-backed Display, owned `Vector`, typed
`OutOfMemoryError`, replaceable `MonotonicClock`, strict HTTPS DNS admission, and selected native
Darwin/GNU declarations, but no network address or resolver actor. The runtime has structured
Execution parking and cancellation, but no cross-thread Wake delivery, retained asynchronous
native callback owner, or event reactor that could make native DNS cancellable.

## Goals / Non-Goals

**Goals:**

- Keep address parsing, domain validation, equality, and presentation ordinary portable Silk.
- Give all result, error, and native-chain ownership one explicit source-level owner.
- Make finite public results and the native provider's unbounded libc behavior impossible to
  confuse.
- Freeze error and precedence rules so socket and HTTP actors can depend on them without acquiring
  compiler privilege or platform policy.

**Non-Goals:**

- DNS caching, TTLs, search suffixes, canonical aliases, reverse DNS, happy-eyeballs ordering, IDNA
  mapping, live-DNS fixtures, UDP packet control, or DNS protocol parsing.
- Async `getaddrinfo_a`, Darwin DNS-SD, blocking-pool integration, cross-thread Wake publication,
  or pretending synchronous libc work can be cancelled after entry.
- HTTP deadline mapping, socket connection deadlines, TLS policy, listeners, or URI parser changes;
  those remain dependent-ticket integrations.

## Decisions

### 1. Three actor modules separate pure values, common policy, and libc ownership

`network_address.silk` owns `Ipv4Address`, `Ipv6Address`, `IpAddress`, `Port`, `Endpoint`,
`DomainHost`, `Host`, validation errors, parsing, equality, slices, `formatInto`, and Display
implementations. Numeric addresses use exact arrays. `DomainHost` uses a private `[u8; 253]` plus
length so construction is allocation-free and never retains a source borrow.

`resolver.silk` owns `FamilySelection`, `ResolveRequest`, `ResolverError`, `ResolvedEndpoints`, and
the `Resolver` service. The service operation accepts only the domain leg. An inherent
`Resolver.resolve` validates the full request, handles numeric hosts itself, samples
`MonotonicClock` only for numeric deadlines, and invokes the replaceable service only for domains.
This shape makes “numeric never invokes the provider” structural rather than conventional.

`native_resolver.silk` owns selected foreign declarations, target constants/layouts,
EAI translation, query construction, addrinfo traversal, and `NativeSystemResolver`. It implements
only the Resolver domain operation. Unsupported targets exclude the entire actor through static
selection.

Keeping parsing inside HTTPS identity was rejected because routing values must be independently
owned and shared by plain TCP, HTTP, TLS, listeners, and tests. Putting libc declarations in the
portable Resolver module was rejected because it would leak native availability into Wasm.

### 2. Requests are constructible but revalidated at the effect boundary

`ResolveRequest.make` consumes a validated Host and checked Port and returns a typed failure when
`maxResults` is outside 1..64. `Resolver.resolve` repeats the capacity check so direct record
construction cannot bypass it. Host and port fields are exposed through actor operations rather
than public mutable storage.

`ResolvedEndpoints` privately owns `Vector<Endpoint>`. Provider-facing actor operations grow only
when admitting a stable-distinct matching-family endpoint and return a status that distinguishes
duplicate, admitted, and one-distinct-too-many. Public operations expose length, indexed copied
endpoints, and a borrowed slice. `Endpoint` is Copy; the vector remains the sole allocation owner.
The common `Resolver.resolve` never trusts a provider's pre-filtering or capacity choice: it
re-admits the returned sequence into a fresh request-bounded result before publication.

A fixed 64-endpoint public array was considered. It would avoid allocation but impose a large
always-present payload and could not exercise the required post-lookup allocation refusal cleanup.
First vector growth after successful native acquisition keeps ownership explicit and makes that
failure path real.

### 3. Address parsing is byte-oriented and presentation has a pure sizing form

IPv4 parsing reads exactly four ASCII decimal components, rejects leading zeroes except the single
digit zero, and accumulates with an explicit 255 bound. IPv6 parsing decodes up to eight 16-bit
groups, records one optional compression position, accepts a strict dotted-quad suffix only at the
end, and expands exactly to sixteen octets. No libc parser participates.

IPv6 formatting first derives eight host-independent 16-bit groups, finds the longest leftmost
zero run of length at least two, and writes lowercase hexadecimal into caller storage. A small
fixed local buffer backs Display, which delegates only the initialized prefix to Writer. IPv4 and
port formatting follow the same no-allocation pattern.

Delegating formatting to `inet_ntop` or parsing to `inet_pton` was rejected because those calls
would make portable value semantics platform-selected and would not prove rejection of libc's
legacy IPv4 spellings.

An ASCII label beginning case-insensitively with `xn--` is additionally decoded with checked RFC
3492 arithmetic to validate its payload syntax and Unicode-scalar range. The admitted source bytes
remain unchanged. This deliberately does not implement Unicode mapping, normalization, or the IDNA
contextual, bidirectional, and code-point-table rules.

### 4. Deadline precedence is decided before each observable branch

The common resolver validates request capacity first. Numeric resolution then checks family and,
when a deadline exists, samples `MonotonicClock.now` exactly once and compares canonical seconds
and nanoseconds; equality is timed out. It never consults the Resolver service.

The native domain implementation inspects the request deadline as its first operation and fails
`DeadlineUnsupported` before building query buffers, reserving result storage, sampling a clock,
or calling libc. A deadline-free call constructs the exact root-terminated nodename and decimal
service on the stack, calls `getaddrinfo` exactly once, and does not introduce a later cancellation
check.

Automatically dropping an expired deadline for native DNS was rejected because that could block
after a caller explicitly asserted an overall bound. Sampling a clock before returning
`DeadlineUnsupported` was rejected because it changes precedence and makes unsupported deadlines
perform observable work.

The portable request actor exposes an owned reconstructed optional deadline for capable providers.
The deterministic capable-provider witness compares it before result allocation or registration,
then uses an affine `Wake` guard and result-owner Drop hook. Native execution covers reached
deadline, parked cancellation, and resumed success, including exact registration, cleanup,
readiness, and completion counters.

### 5. One move-only chain owner brackets all post-lookup exits

Immediately after a successful non-null `getaddrinfo` result, the native module wraps the head in a
private move-only `AddrInfoChain` whose Drop implementation calls `freeaddrinfo`. Traversal borrows
this owner; no helper receives ownership of an interior pointer. Normal return, invalid shape,
limit failure, allocation failure, and any typed propagation all drop the same owner once.

Failed `getaddrinfo`, null-on-success, numeric bypass, and deadline rejection never construct the
owner. Null-on-success is `InvalidNativeResult` and has no chain to free. Manual cleanup branches
were rejected because they duplicate release authority and are especially fragile across
allocation propagation.

### 6. Target layouts and EAI mappings are explicit source data

Darwin and GNU `AddrInfo`, `SockAddrV4`, and `SockAddrV6` declarations follow independent pinned C
fixtures. Darwin and GNU use their actual field order and signed EAI constants. Hints always set
the requested family, stream socket type, TCP protocol, and numeric-service flag.

Traversal validates every node family and the corresponding minimum sockaddr size/pointer before
filtering a known family. It then copies only address bytes, constructs the endpoint with the
requested checked port, performs stable exact-address deduplication, and continues through the
entire chain so a later distinct endpoint can fail the cap. Unknown families and malformed known
records are `InvalidNativeResult`.

EAI_NONAME, EAI_AGAIN, EAI_FAMILY, and EAI_MEMORY map to `NameNotFound`, `TemporaryFailure`,
`FamilyUnsupported`, and `SystemResources`. All remaining statuses map to `NativeFailure`; errno is
captured immediately and stored only for EAI_SYSTEM. `NameNotFound` deliberately describes the
libc result, not a DNS packet or NXDOMAIN observation.

### 7. Deterministic evidence is consolidated and network-free

One focused TypeScript test file builds one portable analysis snapshot for the pure/service
contract and target-specific snapshots only where foreign layouts or selection differ. A generated
Silk acceptance program tables distinct parse/format/domain/result/deadline cases without one
compilation per row. Target C stubs record arguments and return constructed chains for native
execution; no test calls external DNS.

The test reads the three actor sources directly until the shared standard-library manifest and
generated catalog are updated by the integration coordinator. Final CI must analyze the registered
module forms, verify native import absence on unsupported targets, and execute designated stubs in
debug and release.

## Risks / Trade-offs

- **[Synchronous libc can block the host scheduler indefinitely]** → Reject every native domain
  deadline before work and state the limitation in actor/reference documentation; callers that
  require bounded DNS must provide another Resolver.
- **[The endpoint cap can be mistaken for a process-memory/time bound]** → Name the cap as a public
  result bound only and explicitly exclude libc allocation, traversal, NSS, and elapsed time.
- **[Target C layouts or EAI constants drift]** → Pin independent per-target C/header fixtures and
  keep Darwin/GNU declarations and mapping branches visible in one native actor.
- **[A direct record literal can forge request invariants]** → Revalidate at `Resolver.resolve`
  before numeric publication or provider dispatch.
- **[Repeated domain grammar diverges from HTTPS identity]** → Use the same settled LDH/error
  precedence and add cross-actor acceptance rows; a later shared actor may replace duplication only
  as a clean breaking change, not a compatibility shim.

## Migration Plan

Register the three new source actors and generated catalog entries atomically with their focused
tests and reference page. Existing APIs are unchanged. Dependent socket and HTTP changes then adopt
`Host`, `Endpoint`, and `Resolver.resolve`; no fallback or legacy address representation is retained.
Rollback removes the new modules, registrations, tests, docs, and this unarchived change together
before dependent tickets land.
