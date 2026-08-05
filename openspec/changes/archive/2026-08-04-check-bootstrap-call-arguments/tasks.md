## 1. Publish argument facts

- [x] 1.1 Add ordered argument identities and fact shapes retaining integer or parameter-reference expressions and exact syntax provenance
- [x] 1.2 Collect arguments in concrete source order while preserving parser ownership for missing and damaged syntax
- [x] 1.3 Add fixtures and tests for literal, parameter-reference, multiple, and recovered arguments

## 2. Check the positional call contract

- [x] 2.1 Add positional argument-to-target-parameter mapping facts for uniquely resolved calls
- [x] 2.2 Add compatible, arity-mismatch, and unavailable call-contract outcomes with complete reason data
- [x] 2.3 Retain partial ordinal mappings on arity mismatch and withhold mappings for unresolved targets
- [x] 2.4 Emit one deterministic `SEM0007` per uniquely resolved wrong-arity call without cascading on unavailable prerequisites

## 3. Prove contract behavior

- [x] 3.1 Test zero-, one-, and two-argument compatible calls plus too-few and too-many calls
- [x] 3.2 Test unavailable parameter types, unavailable argument references, missing targets, ambiguous targets, and recovered syntax
- [x] 3.3 Verify call-contract results remain distinct from expression type and function return compatibility across repeated analysis

## 4. Make call contracts visible

- [x] 4.1 Add compatible, too-few, too-many, unavailable-type, and unresolved-call presets to the Syntax Inspector
- [x] 4.2 Render arguments, positional mappings, unmatched items, expected and actual counts, contract state, and `SEM0007`
- [x] 4.3 Verify accessible relationship labeling and desktop and narrow-viewport behavior in the live inspector

## 5. Document and verify

- [x] 5.1 Update compiler README, public examples, changeset, and release-candidate validation for argument facts and call contracts
- [x] 5.2 Run focused compiler and docs tests, then `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`
