## Context

Entry discovery, process status, failure eligibility, traces, runtime adapters, and embeddings currently disagree. They must converge on one target-neutral outcome before adapters decide how to render or exit.

## Goals / Non-Goals

**Goals:** confirmed entry shapes; no `Report`; stable statuses; structured outcomes; logical traces; standalone/embedded separation; pay-for-use.

**Non-goals:** user-defined error formatting, trap recovery, implicit dependency providers, scheduler semantics, or cleanup during fatal traps.

## Decisions

1. Resolve and validate the public entry contract before target planning.
2. Use one closed termination outcome with success, typed-failure, and trap variants plus provenance and logical frames.
3. Build logical frames from semantic call identities, not physical backend frames, and preserve them through suspension.
4. Let standalone adapters map the outcome to status/rendering; embeddings receive data only.
5. Compute adapter/runtime linkage from reachable entry and reporting needs.
6. Delete `Report` and member-ordinal reporting paths rather than adapting them.

## Risks / Trade-offs

- Optimized trace stability requires explicit logical metadata and may modestly increase artifacts that can fail.
- Trap paths retain no-unwind semantics, so structured trap data cannot claim cleanup that did not run.

## Migration Plan

Introduce the outcome in evaluation, align entry validation/statuses, migrate MIR/backend propagation, migrate native/CLI/embedding adapters, delete `Report`, add pay-for-use checks, and update all terminal tests/docs.

## Open Questions

Future error-formatting interfaces may enrich rendering but cannot affect entry eligibility or structured identity.
