## Context

Once catalogs, runtime inventories, and structured termination are explicit, the driver can validate their identities before failures escape into source analysis or backend execution.

## Goals / Non-Goals

**Goals:** stable artifact identities; early matched-set validation; owning-boundary error classification; CLI/embedding parity.

**Non-goals:** network updates, package resolution, compatibility negotiation, legacy fallback, or target emulation.

## Decisions

1. Compute deterministic content digests during artifact generation and publish a toolchain identity graph.
2. Validate compiler/catalog/source/intrinsic identities before resolving user source.
3. Validate target-provider/runtime coverage after reachable intrinsic planning.
4. Represent integrity and operational failures as structured driver outcomes distinct from program termination.
5. Fail closed on missing or mismatched promised artifacts; do not search alternate versions implicitly.

## Risks / Trade-offs

- Strict matching makes stale generated artifacts immediately visible, which is intentional but affects local workflows.
- Digest inputs must exclude nondeterministic paths and timestamps.

## Migration Plan

Define identity records, add deterministic generation, validate frontend distribution, validate target/runtime subset, expose structured driver outcomes, update CLI/embeddings/tests, and remove ad hoc missing-artifact fallbacks.

## Open Questions

Version display and release-channel policy are packaging concerns outside the integrity contract.
