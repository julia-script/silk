# Tasks

## 1. Body Queries

- [x] 1.1 Route `Semantic.checkBody` through the typed session runtime with canonical request identity, nested read observations, memoization, and forced-fresh execution.
- [x] 1.2 Preserve `BodyQuery` cross-revision artifact validation and current-presentation reuse beneath the session provider.

## 2. Evaluation Queries

- [x] 2.1 Add `Semantic.evaluate` and `evaluateFrom` with value-sensitive evaluation identity and live dependency observation.
- [x] 2.2 Migrate residualization and static evaluation callers without changing budgets, recursion, cached-failure, or retry behavior.

## 3. Verification and Documentation

- [x] 3.1 Add focused fixtures for within-session body reuse, forced-fresh equivalence, value-separated evaluation, recursion, budgets, defects, and editor reuse.
- [x] 3.2 Update exports and compiler architecture documentation for body/evaluation query ownership.
