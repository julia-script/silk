## Why

Silk can represent an explicit certificate authority, but it cannot yet assemble those authorities into an independently owned bounded trust set or replace the source used by future TLS connections. Without that boundary, applications risk borrowed-storage invalidation, accidental deduplication of differently constrained authorities, or implicit platform-policy behavior.

## What Changes

- Add `silk/trust_snapshot` with opaque immutable snapshots, finite count and encoded-byte limits, strict PEM construction, independent copy, and order-preserving combination.
- Add `silk/trust_source` as a lexical replaceable service that loads one independently owned snapshot per request.
- Add `silk/memory_trust_source` with allocation-free atomic replacement and copied loads that survive later replacement or provider drop.
- Add the consuming `CertificateBundle.intoCertificates` accessor so PEM decoding transfers owned certificates into unconstrained `TrustAnchor` values without reparsing or DER copying.
- Register and document the ordinary-source actors and add economical ownership, native, and LLVM-to-Wasm evidence with exact pull-request CI selection.
- Keep native file acquisition, OS policy, path validation, TLS records/client/networking, caches, watchers, TTL, and environment discovery outside this change.

## Capabilities

### New Capabilities

- `owned-trust-snapshots`: Bounded owned trust sets, strict PEM import, independent snapshot operations, lexical trust-source loading, and replaceable in-memory trust configuration.

### Modified Capabilities

None.

## Impact

This adds ordinary Silk source under `packages/compiler/stdlib/silk`, one consuming operation on `CertificateBundle`, canonical standard-library registrations, compiler ownership/requirement assertions, a shared native corpus case, a compact Wasm witness, generated API/reference output, and focused documentation. It consumes JUL-185's `TrustAnchor` unchanged and provides the portable snapshot input for JUL-186 and JUL-187 without introducing any native I/O or trust-policy equivalence.
