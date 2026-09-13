## Context

The generic `Uri`/`UriReference` actors already provide allocation-free borrowed parsing, explicit
`OwnedUri`/`OwnedUriReference` storage, lossless lexical components, and allocator-backed copies.
The standard-library manifest has no HTTP actors. Existing test infrastructure provides structured
analysis, deterministic allocator providers, and one shared native acceptance
corpus; repository policy requires using those cheapest adequate tiers. See `proposal.md` for
motivation and `specs/http-values/spec.md` for normative behavior.

## Goals / Non-Goals

**Goals:**

- Make every scalar and collection valid by construction, byte preserving, bounded, and usable by
  future parsers, clients, and servers without duplicate interpretation.
- Keep borrowed views allocation-free and make ownership changes explicit, affine, and observable
  to Silk's lifetime analysis.
- Centralize overflow-safe limits, Host/authority rules, metadata grammar, and field-only formatting
  while keeping public failure channels closed and precise.
- Keep implementation target-neutral ordinary Silk, with allocation as the only effectful boundary.

**Non-Goals:**

- Wire-head parsing, start-line or complete-head serialization, body framing, trailers, content
  length or transfer precedence, content decoder selection, connection reuse, sockets/TLS,
  proxy/network scheme policy, DNS/IDNA, or complete client/server behavior.
- Compatibility aliases, unchecked constructors, unlimited defaults, generic header concatenation,
  or compiler-known HTTP declarations.

## Decisions

### Split the capability into three public actors

`silk.http` owns scalar values, errors, fields, and request/response heads;
`silk.http_headers` owns ordered collections, limits, lookup/metadata iteration, ownership, and
field formatting; `silk.http_target` owns target forms, HTTP authority, URI conversion, Host
validation, and effective-authority selection. The three actors import one another where their
public data shapes meet; Silk resolves the declarations as one ordinary source graph without a
fourth support module or compiler privilege.

One large `http.silk` alternative would hide ownership and target-policy seams and make future
parser/framing work contend in a single file. Per-type modules were also rejected because these
values form three cohesive actors and would multiply public registration and import noise.

### Store borrowed values as validated source plus ranges

`Method`, `Header`, request-target variants, authority components, and optional reason phrases keep
their source borrow and validated scalar ranges. Public fields stay private. Formatting immutable
lexical values returns their original borrow where possible; conversions that synthesize text use
checked caller output. A parser can therefore trim wire OWS once before calling `Header.make`, but
no later consumer can introduce CRLF or reinterpret obs-text.

Copying each scalar eagerly was rejected because it would make parsing allocate and duplicate
ownership logic. Storing only normalized enums was rejected because it would lose extension methods,
unknown codes/tokens, original field case, and exact octets.

### Represent owned collections as bytes plus offsets, never self-references

`OwnedHeaders` owns contiguous payload storage and an owned record sequence describing name/value
ranges. `OwnedRequestHead` and `OwnedResponseHead` extend the same principle to method, target, and
reason payloads. A `view` borrows the owner and carries a representation discriminator for either a
caller-provided `&[Header]` or owner bytes plus records. Iterators resolve one `Header` value on
demand from the active backing representation; they do not allocate or synthesize a borrowed slice
of header structs.

A self-referential owned value is impossible to move safely under Silk's affine model. One allocation
per field would simplify offsets but increase failure/cleanup states and weaken the explicit total
owned-byte budget. A generic owned wrapper per scalar adds API without improving lifetime safety;
standalone scalars stay borrowed and heads/collections are the ownership unit.

### Validate and size before mutation or allocation

Pure constructors perform token, byte, form, field-count, and checked aggregate validation first.
Each format operation has a pure sizing pass using checked addition. It returns `SizeOverflow` or
`OutputTooSmall` before the first output write. Copy operations derive exact payload and record
requirements, account for capacities plus index storage against `maxOwnedBytes`, and reject invalid
or over-limit input before acquisition.

Streaming partial output and reserve-as-you-go alternatives were rejected: field serialization is
small enough to size first, and mutation atomicity prevents callers from accidentally transmitting a
prefix after failure.

### Acquire owned pieces under one cleanup boundary

Owned copies allocate payload and record storage through the existing `Allocator` service and
existing owned storage actors. The implementation maintains acquired pieces only inside an effectful
construction scope; if a later deterministic acquisition fails, affine drop of the in-progress
owner releases all prior pieces. No partial public owner is returned, semantic `ValueError` remains
inside `Result`, and allocation failure remains `OutOfMemoryError` on the effect channel.

Collapsing allocation failure into `ValueError` was rejected because it erases the established URI
and allocator contract. Manual duplicated cleanup branches were rejected because they are fragile
under future acquisition changes.

### Use one explicit Limits value and checked counters

`Limits` carries `maxMethodBytes`, `maxTargetBytes`, `maxNameBytes`, `maxValueBytes`, `maxFields`,
`maxFieldBytes`, and `maxOwnedBytes`, all required. Operations accept the full value when several
limits apply and accept the named scalar bound for standalone method/target parsing. A shared private
checked-add routine maps representation overflow to `SizeOverflow`; limit failures identify the
specific limit with allowed and attempted counts. Checks occur before the acquisition or write that
would cross a bound.

Hidden defaults and an unlimited sentinel were rejected because they make upstream parser/client
budgets non-auditable. Saturating arithmetic was rejected because it confuses overflow with a real
limit comparison.

### Reuse generic URI syntax while applying HTTP-specific authority policy once

Absolute-form and URI conversion parse through existing `Uri`/`UriReference` accessors, then apply
HTTP restrictions: no userinfo, no fragments on raw targets, CONNECT requires a nonempty host and a
numeric 1–65535 port, and `*` is exclusive to OPTIONS. `HttpAuthority` preserves bracketed IP
literals and distinguishes absent port from empty/invalid port. Origin conversion writes `/` for an
empty path and retains a present empty query. URI conversion deliberately omits fragments; raw target
parsing rejects them.

Duplicating the RFC 3986 parser was rejected because it would create divergent percent/authority
interpretation. Treating every URI as an HTTP(S) network destination was rejected because generic
absolute-form may carry other schemes and network admission belongs to clients/proxies.

### Validate Host cardinality separately from effective-authority selection

`validateRequestHost` finds Host through ordered case-insensitive lookup, validates at most one field
as `HttpAuthority`, and requires it only for HTTP/1.1. `effectiveAuthority` uses the target authority
for absolute/CONNECT forms and the validated Host for origin/asterisk forms. A different but valid
Host does not invalidate absolute-form or CONNECT. `hostForUri` serializes URI authority without
userinfo; callers own explicit-override comparison.

Universal string equality, default-port normalization, and DNS equality were rejected because server
routing, client override validation, and proxy forwarding have different policies.

### Parse metadata lazily over ordered raw fields

Metadata functions return bounded iterators that scan matching header fields in insertion order and
yield source-borrowed tokens. Connection and Content-Encoding share token-list mechanics but reject
parameters. Transfer-Encoding has a distinct state machine for quoted transfer parameters. Errors
carry the original field index and value-relative byte offset. Known values are optional recognition
helpers over the preserved spelling; unknown valid tokens remain values.

Eager normalization or closed enums were rejected because both erase extension metadata. Generic
comma concatenation was rejected because field combination rules are field-specific and unsafe for
Set-Cookie.

### Prove claims at the cheapest adequate test tier

One focused TypeScript test file builds one shared analysis snapshot for the complete behavioral
program plus smaller ownership and source-navigation fixtures. These HTTP operations are ordinary
runtime functions, not static functions, so they make no compile-time execution claim for
`StaticEvaluation` to prove. Ownership fixtures prove borrowed/owned escape rules. Deterministic
allocator providers will sweep acquisition ordinals and confirm cleanup structurally. A selected
memory-only program joins the existing shared native acceptance corpus, which already supplies the
cross-target runtime oracle; no bespoke HTTP binaries or per-feature determinism tests are added.

Pinned Zig source fixtures record comparison provenance and intentional differences. A distinct
test-economics review checks that assertions are not duplicated across tiers, while an independent
correctness review checks the final contract and implementation.

## Risks / Trade-offs

- [Owned-byte accounting diverges from actual capacities] → centralize record-layout sizing, use
  checked arithmetic, and test exact/one-past boundaries plus each deterministic allocation ordinal.
- [Iterator lifetimes accidentally detach from owner storage] → keep constructors private, derive
  every yield from the iterator's active borrow, and add negative ownership-analysis fixtures.
- [Target forms overlap lexically] → select special method forms first, parse absolute syntax through
  `Uri`, and table-test ambiguous delimiters, empty paths/queries, and bracketed literals.
- [Metadata grammar becomes framing policy] → expose only validated tokens/parameters and preserve
  unknowns; leave content length, precedence, decoding, and reuse decisions to dependent issues.
- [Three actors form an import cycle] → keep the cycle declaration-only at its seams: headers and
  targets depend on scalar types from `silk.http`, while heads depend on their already-validated
  collection and target values; verify the complete three-module graph in every maintained profile.
- [Large fixtures slow the compiler suite] → share one source analysis per table and add only selected
  runtime cases to the existing corpus.

## Migration Plan

This is an additive green-field capability with no compatibility contract. Add and verify ordinary
source modules and tests first, register the manifest entries, regenerate public surfaces, then update
the prescriptive reference and examples. If validation fails before publication, remove the new
actors and generated entries together; no persisted data or deployed protocol state requires
migration.
