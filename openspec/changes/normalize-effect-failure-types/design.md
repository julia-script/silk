## Context

Failure binders, row wrappers, ordinary unions, propagation, and catch currently use overlapping representations. The migration must be atomic because every intermediate dual model would leak into generic signatures and standard-library APIs.

## Goals / Non-Goals

**Goals:** represent failure `E` as an ordinary type; share union algebra and ownership; generalize selective recovery; migrate error naming.

**Non-goals:** trap recovery, exception throwing, dynamic type tests, or compatibility aliases.

## Decisions

1. Remove the failure-row type kind and parse Effect channel labels separately from type parameters.
2. Reuse canonical structural-union normalization and `never` for empty failures.
3. Carry ordinary owned values in outcome failure variants; no `Row<!E>` materialization remains.
4. Express catch selection as static membership plus `Without<E,S>` and preserve residual concrete types through specialization.
5. Migrate every shipped error declaration and source use to `*Error` in the same change.

## Risks / Trade-offs

- This touches many semantic caches and golden encodings; deleting the old kind before adapting every consumer is essential.
- Ordinary unions must not accidentally admit escaping borrow payloads; detached ownership remains the gate.

## Migration Plan

Introduce ordinary failure facts behind the existing syntax, migrate inference and outcomes, migrate catch and stdlib, rename errors, update diagnostics/tests/docs, then delete old row nodes and encodings.

## Open Questions

None that can reverse the direction. Physical union encoding remains owned by the structural-union plan.
