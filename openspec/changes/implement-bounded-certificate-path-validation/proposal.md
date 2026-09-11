## Why

Silk can decode and inspect certificates, but it cannot yet construct a bounded chain from peer input to explicit trust authority. TLS needs a deterministic validator that proves the selected certificate path under Silk's restricted server profile without hidden clocks, I/O, or unbounded work.

## What Changes

- Add the ordinary-source `silk/certificate_path` actor with explicit validation limits, stable owned errors, and a borrowed validated-path result.
- Construct paths with bounded iterative depth-first search, anchor-first deterministic traversal, exact caller-order indices, duplicate handling, and global work counters.
- Validate retained certificate signatures, one explicit caller time, path length, and cumulative DNS/IP name constraints under the fixed certificate profile.
- Add deterministic fixture provenance, economical native/ownership/Wasm evidence, generated API documentation, and CI selection for the native acceptance case.

## Capabilities

### New Capabilities

- `bounded-certificate-path-validation`: Deterministic, resource-bounded construction and validation of a TLS-server certificate path against explicit constrained trust anchors.

### Modified Capabilities

None.

## Impact

The change adds one public standard-library module and its generated reference page. It extends the compiler standard-library manifest, shared acceptance corpus, focused ownership and target evidence, deterministic fixture assets, and pull-request native-smoke selection. It consumes `silk.certificate_profile`, `silk.trust_anchor`, the existing signature primitives, `silk.system_clock.Instant`, and the caller-provided `Allocator`; it adds no compiler-known actor or platform capability.
