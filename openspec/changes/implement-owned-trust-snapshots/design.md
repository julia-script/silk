## Context

See [proposal.md](proposal.md) and [the delta specification](specs/owned-trust-snapshots/spec.md). The exact work base is JUL-185 head `e545f7c0ca2c87a8949fa4c8b34cafb8e0d64e9c`, which supplies opaque owned `TrustAnchor`, independent `TrustAnchor.clone`, and checked `TrustAnchor.encodedBytes`. The existing strict `CertificateBundle.decodePem` already performs atomic PEM/DER decoding and preserves order and duplicates, but it exposes only borrowed indexed access.

Ordinary `.silk` sources are not reliably represented by the repository graph, so the design is grounded in direct source, manifest, test, and generator inspection. The service and snapshot boundary must remain portable to raw Linux and LLVM-to-Wasm; the native file child owns every filesystem and target-policy concern.

## Goals / Non-Goals

**Goals:**

- Give each load and TLS connection a complete independently owned, bounded trust set.
- Preserve every authority candidate, restriction, duplicate, and ordering decision exactly.
- Make source selection lexical and testable, with allocation-free replacement of complete memory state.
- Keep typed semantic failures distinct from allocator refusal and ensure partial owners never escape.

**Non-Goals:**

- Native PEM file reads, OS root discovery, keychain extraction, distribution policy, caches, refresh, watchers, TTL, or environment lookup.
- Certificate profile inspection, path search, signature/time/name validation, revocation, CT, or network/TLS state.
- Deduplication, restriction intersection, platform synchronization, global service installation, or cross-thread sharing guarantees.

## Decisions

### Store an opaque vector and preflight it without allocation

`TrustSnapshot` owns one private `Vector<TrustAnchor>`. `fromAnchors` walks the borrowed vector view to validate count and checked aggregate `TrustAnchor.encodedBytes`, then moves the already-complete vector into the snapshot. The constructor allocates nothing and accepts a zero-length vector when both limits permit it.

Alternative considered: copy the input vector inside every constructor. Rejected because it adds an unnecessary allocator boundary, prevents an infallible ownership transfer after validation, and complicates rollback without increasing safety.

### Transfer decoded certificates before constructing anchors

`CertificateBundle.intoCertificates` consumes the bundle, destructures its private owner, and returns
that vector directly. `fromPem` strictly decodes once, consumes the bundle, reserves an exact
anchor-vector shape only after layout/count checks, moves each certificate out, wraps it with
allocation-free `TrustAnchor.fromCertificate`, and delegates final accounting to `fromAnchors`.

Alternative considered: call `Certificate.copy` through the bundle's borrowed getter. Rejected because PEM decoding already owns the complete certificate values; copying DER would add work, allocation-failure points, and duplicate byte ownership solely to recover ownership hidden by an accessor.

### Preflight copy and combine before cloning

Copy and combine first validate final count and encoded-byte totals against limits and representation bounds. They then reserve exact output capacity and clone anchors in order. A local output vector owns every completed clone, so lexical destruction releases partial state on allocation failure. Inputs remain borrowed and unchanged. `combine` treats entries as alternative authorities and never merges identical DER because configured restrictions can differ.

Alternative considered: append and stop when a bound is reached. Rejected because it performs avoidable allocations before a deterministic semantic failure and makes the first failure depend on allocator behavior.

### Model a lexical service and a separate memory provider actor

`silk.trust_snapshot` owns limits, snapshots, and the complete public error family.
`silk.trust_source` owns only the `TrustSource.load` service contract, and
`silk.memory_trust_source` owns the concrete provider. The provider's mutable receiver serializes
loads; each load delegates to `TrustSnapshot.copy`. `replace` uses the existing sealed
`Intrinsic.replace` primitive to exchange complete snapshots with no allocation or failure.

Alternative considered: put provider state and constructors on the service module. Rejected because the service is the capability contract while memory state is one replaceable actor; keeping them separate lets JUL-186 add native acquisition without turning one module into a provider grab-bag.

### Define the full shared error vocabulary now but use it narrowly

`TrustSourceError` is one owned wrapper with semantic variants for `Decode`, `LimitExceeded`, `InvalidConfiguration`, and `File`. JUL-170 constructs only decode and snapshot limit failures. The configuration/file variants establish the stable service boundary consumed by JUL-186 without performing I/O here. `OutOfMemoryError` remains a separate Effect failure as specified.

Alternative considered: expose `DecodeError | SnapshotLimitError` now and introduce a wrapper in JUL-186. Rejected because it would force callers and the service signature through an immediate breaking rewrite and would not preserve one semantic recovery boundary across portable and native sources.

### Consolidate security and ownership evidence

One shared native corpus program uses deterministic committed certificate fixtures and one bounded
allocator-failure sweep to distinguish atomic rollback boundaries. One structured analysis test
holds the ownership, requirement-row, cleanup, and non-forgeability claims. One compact existing
driver test compiles/evaluates a reduced source on LLVM-to-Wasm. CI's pull-request native smoke list
explicitly includes the corpus case so the exact draft head supplies full-suite evidence.

Alternative considered: one native program per acceptance bullet. Rejected because every program repeats compiler and linker startup while adding little independent signal.

## Risks / Trade-offs

- [Exact reservation or cloning can expose new allocator ordinals] → Sweep only distinguishing boundaries in the consolidated native case and verify no partial state escapes.
- [A future provider could misuse the broad error family] → Document stable variant meaning and keep each implementation limited to failures it can actually produce.
- [Copying every load costs O(anchor count plus retained bytes)] → This is the deliberate independent-owner contract; callers control load frequency and finite limits, and no cache is hidden in the service.
- [The consuming bundle accessor expands a decoder actor] → Keep it allocation-free and semantic-free; it assigns no trust and exists only to recover the already-owned sequence.
- [Generated surfaces overlap other certificate tickets] → Regenerate from the canonical manifest and integrate registrations serially in the final stack.

## Migration Plan

Add the consuming bundle accessor, three actors, manifest registrations, tests, documentation, and generated output atomically. There is no existing trust-source API or persisted format to migrate. Rollback removes the new actors and registrations and restores the prior generated surfaces; no compatibility shim or fallback is retained.
