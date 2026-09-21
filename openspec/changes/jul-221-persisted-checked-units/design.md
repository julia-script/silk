# Design

## Context

See proposal.md for motivation and the persisted-checked-units spec for behavior. JUL-217/218 made
`SemanticQuery.Completed` a bounded record containing a reconstructible descriptor, stable result
fingerprint, answer, and ordered query/input observations. JUL-219 supplies bounded atomic Storage.
The remaining gap is a strict transfer format and a startup path that offers loaded `CheckBody`
records to the existing validator without making disk bytes a second reuse authority.

## Goals / Non-Goals

**Goals:**

- Persist one useful family—complete checked units—without generalizing disk caching to every query.
- Atomically bind the answer and dependency manifest to exact schema/compiler/query identities.
- Decode under explicit structural and byte bounds, validate every internal and stable reference,
  and expose no partial unit.
- Admit loaded and in-memory candidates through the same recursive validator and current presenter.
- Fail open for optional cache data while preserving invalid configuration, interruption, and defect
  semantics.

**Non-Goals:**

- Persisting HIR, headers, evaluation outcomes, instance graphs, layouts, MIR, or backend emission.
- A global candidate index, multi-key transactions, eviction, deletion, janitors, or cache routing.
- Treating checksums as producer authenticity or stored ordinals as stable identities.
- Claiming Milestone C1/C2 HIR persistence is complete.

## Decisions

### One envelope contains answer and ordered dependencies

Each stable `CheckBody` address maps to one envelope containing address/codec schema versions,
compiler semantic identity, exact query descriptor/key, result fingerprint, ordered observations,
and the entire checked unit. A digest covers the canonical envelope payload. Storage publishes this
one record atomically; there is no separately readable manifest that can drift from its answer.

The alternative—one answer record plus dependency sidecars—would require a transaction or admit
mixed generations after concurrent publication or partial eviction.

### Loaded records enter the existing snapshot path

Semantic startup may read a candidate for a body only after current HIR, selection, headers,
resolution, and presentation exist. A successfully decoded record becomes an ordinary prior
`SemanticQuery.Completed<CheckedUnit>` candidate. The existing validator replays its ordered
observations; an unavailable child record executes the current provider. Only a validator hit is
published into the current session and projected through `BodyQuery.reuse`.

The alternative—a special disk-hit branch inside `Semantic.checkBody`—would bypass recursive
dependency validation and create a second admission authority.

### Persistence is provider-selective and demand-driven

Only revision-reusable `CheckBody` completions are eligible. On demand, the semantic provider first
consults its in-memory previous snapshot; when absent it may load the exact Storage address and
offer that candidate to the same validator. Completed executions publish eligible records after
the current answer and observations are frozen. Aborts, cycles, forced-fresh demands, and
cache-disabled sessions neither read nor publish persisted records.

Header/name queries may be reconstructed while validating a loaded unit; their answers need not be
persisted. This keeps the first slice bounded and is why C1/C2 HIR disk caching is an optimization,
not a correctness prerequisite.

### The checked-unit codec is strict and stable-reference based

Extend `TirCodec` around a versioned complete-unit root rather than casting parsed JSON. Decode with
explicit limits for total bytes, nesting depth, collection entries, strings/byte arrays, numeric
forms, and identifier ranges. Closed tagged unions reject unknown tags and extra/missing fields.
Every declaration/artifact reference is encoded through a stable identity dictionary and resolved
through the current declaration index; serialized ordinals are never looked up by current slot.
Hidden bodies name and validate their parent owner. Node, local, evidence, cause, provenance, and
table references must resolve within the complete envelope before any body is published.

The alternative—hardening only the current recursive generic decoder—would still accept structurally
impossible TIR graphs and positional reference redirection.

### Optional-cache failures are observable but not semantic failures

Candidate outcomes distinguish missing, stale, incompatible, corrupt, oversize, reference-invalid,
external-read failure, and publication failure. The first seven recompute; external read failure
also recomputes after recording a typed cache-failure event, and publication failure skips the
write after preserving the valid computed answer. Invalid logical address/limit is configuration
failure. Interruption and unexpected defects propagate. Reports and traces distinguish lookup,
validation, reuse, recomputation, rejection/failure, and publication.

## Risks / Trade-offs

- [Risk] A broad TIR graph makes strict validation large. → Use a closed complete-unit schema,
  central reference dictionaries, and fail the entire candidate on the first invalid reference.
- [Risk] Current presentation may accidentally retain persisted spans. → Encode anchors/provenance,
  omit source spans, and route every admitted unit through the existing current presenter.
- [Risk] Missing dependency records could be mistaken for unchanged. → Preserve reconstructible
  observations and let the shared validator execute current child providers when no prior answer is
  available.
- [Risk] Optional I/O recovery may swallow interruption or defects. → Recover only typed external
  Storage read/publication reasons; keep configuration failures, interruption, and defects outside
  that recovery branch.

## Migration Plan

1. Define bounded codec limits, stable reference dictionaries, complete-unit encode/decode, and
   typed rejection reasons; replace the unsafe single-body decode path and update all callers.
2. Define the versioned persisted semantic envelope and strict ordered observation codec.
3. Add explicit semantic persistence configuration, on-demand candidate loading, shared-validator
   admission, eligible publication, and cache-disabled/fresh bypass.
4. Add a shared restart harness covering unchanged, changed, moved, removed, repaired, selected,
   hidden, rejected, corrupt, and failure cases.
5. Update compiler architecture/trace documentation and delete superseded codec or persistence
   paths in the same change.
