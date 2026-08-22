## Context

See proposal.md. All four reconciliation targets live in the pure type/ownership layer (no Effect, no node:*), so the work is deterministic and is covered by the existing golden and fresh-process determinism suites.

## Goals / Non-Goals

**Goals:** make every duplicated semantic rule resolve to exactly one owner; keep every reconciliation behavior-preserving except the access-satisfaction fix; pin the access rule in spec and in tests.

**Non-Goals:** structural module splits (separate changes); boundary/Effect hygiene; dead-code removal.

## Decisions

### The access partial order (Shared < Exclusive < Take)

Four encodings exist today. Decision: adopt the rank ordering already used by `InterfaceWitnessCompatibility.ts:181` (`accessRank(requirement.access) <= accessRank(allowed.access)`) as canonical, and correct `Type.ts:1682` (`haveSameRepresentationShape`, exact-only) to use the shared helper. Rationale: exact-only rejects a strictly larger set than every other path, and the three non-outlier sites agree on "stronger satisfies weaker". Alternatives: making everything exact-only was rejected — it would contradict the documented Shared/Exclusive examples in `bootstrap-service-declarations` and reject witness selections the compatibility path already accepts.

Add to `Type.ts`: `accessRank(access): 0 | 1 | 2`, `compareAccess`, and `requirementSatisfies(supplied, required): boolean`. Route `TypeCompatibility.ts:66,81,94`, `InterfaceWitnessCompatibility.ts:84,181`, and the `Type.ts` sites (`1682,3046,3250,3264`) through them.

### LIFO release-order helper

Add an exported `inReleaseOrder(owned)` helper in `Ownership.ts` near `cleanupPlan` (it reuses cleanup-plan shapes): returns owned entries in reverse capture order, filtering `NoCleanup`. `SuspensionOwnership.ts:362` and `Lower.ts:5819,5986` import it and delete their local `.reverse()` + dedup variants. Keep the most conservative deduplication (dedupe by ordinal) so a release is never double-issued.

### Set helpers

Add `internal/SetOf.ts` (`equal`, `union`, `intersection`) and route `Ownership.ts:2680,2682` and `SuspensionOwnership.ts:72,78` through it.

### `contains*` via one boolean fold

Add `Type.some(self, predicate): boolean` (a boolean variant of the existing `fold`) and re-express the five `contains*` predicates as one-liners, choosing the most conservative recursion (recurse into `requirements`) so no predicate silently widens acceptance.

### `Token.describe('Invalid')`

Change the description from `'valid token'` to `'invalid byte'`.

## Risks / Trade-offs

- [Access change widens acceptance] → new scenario tests pin the exact before/after; `bootstrap-service-declarations` examples are the source of truth for the ordering.
- [`.reverse()` dedup differences] → keep the most conservative dedup (by ordinal) so releases are never double-issued.
- [Conservative `contains*`] → choosing the widest recursion could reject programs; verify against the full suite before merging.

## Validation

`pnpm typecheck`, then `pnpm exec biome check .`, then `pnpm test`. Add `@effect/vitest` cases for each Shared/Exclusive/Take pair and confirm the new scenarios in the spec each map to a passing test.
