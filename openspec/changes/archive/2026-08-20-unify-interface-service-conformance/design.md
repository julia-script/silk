## Context

Interfaces and services evolved through separate declaration and witness paths. Services need runtime requirement eligibility, but that does not justify separate static operation semantics.

## Goals / Non-Goals

**Goals:** one canonical contract and witness model; implicit `Self`; mixed implementation forms; coherence and locality; one eligibility check for services.

**Non-goals:** dynamic dispatch, ambient dependency lookup, orphan conformances, or implicit default services.

## Decisions

1. Lower both declaration forms into one static contract record plus a service-eligibility bit.
2. Represent `Self` explicitly in substituted operation contracts but keep it implicit in source syntax.
3. Resolve inline bodies and mapped functions into one ordered witness table.
4. Publish a witness only after completeness, signature, visibility, locality, overlap, and termination checks pass.
5. Requirement construction consults the eligibility bit; all other generic and static-call machinery ignores declaration kind.

## Risks / Trade-offs

- Existing completed witness changes contain narrower assumptions and must be migrated, not layered underneath.
- Conjunction and overlap checks can increase specialization work; proofs remain finite and deterministic.

## Migration Plan

Create the unified contract fact, migrate interface bounds, migrate service declarations, migrate conformances and static calls, update requirement eligibility, then delete duplicated service/provider witness identities.

## Open Questions

None at the programmer-model level. Exact internal witness key layout is an implementation decision.
