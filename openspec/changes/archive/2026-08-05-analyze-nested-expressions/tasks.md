## 1. Make semantic expressions recursive

- [x] 1.1 Replace the flat argument-expression fact boundary with a discriminated recursive expression fact owned by the existing semantic actors
- [x] 1.2 Preserve stable nested call and argument identities, exact spans, and source-ordered provenance without cyclic object references
- [x] 1.3 Update public exports and compile-time exhaustiveness coverage for the prerelease breaking fact shape

## 2. Analyze nested calls

- [x] 2.1 Recursively analyze integer, parameter-reference, and call expressions using the enclosing function's parameter environment
- [x] 2.2 Resolve each nested target and compute nested positional mappings, contracts, result types, and dependent outer states from the leaves outward
- [x] 2.3 Preserve phase-owned recovery and deterministic diagnostic ordering for missing, ambiguous, damaged, wrong-arity, and type-unavailable inner expressions
- [x] 2.4 Add semantic fixtures and tests for one nested call, ordered sibling calls, unavailable inner facts, repeated analysis, and representative deep input

## 3. Keep evaluation honest between milestones

- [x] 3.1 Add the closed unsupported-nested-expression blocked reason with exact nested identity and source provenance
- [x] 3.2 Block only when evaluation reaches a nested expression, retaining the deterministic trace prefix and ignoring unreachable nested syntax
- [x] 3.3 Add evaluator tests for reachable, unreachable, and repeated transitional outcomes while preserving existing flat evaluation behavior

## 4. Make recursive facts visible

- [x] 4.1 Add valid, unresolved, incompatible, and syntax-damaged nested semantic presets to the hidden Syntax Inspector
- [x] 4.2 Render nested semantic cards, contracts, types, candidates, unavailable dependencies, and source links from compiler-produced facts
- [x] 4.3 Render the temporary blocked evaluation reason beside the inspectable nested facts and verify the page manually at desktop and narrow widths

## 5. Document and verify

- [x] 5.1 Update the semantic and inspector documentation for recursive expression facts and the temporary evaluation boundary
- [x] 5.2 Add the required changeset and verify release-candidate contents if public package types or exports change
- [x] 5.3 Run focused semantic, evaluator, and docs tests, then `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`
