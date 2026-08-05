## 1. Collect parameter declarations

- [x] 1.1 Add source-local parameter identities and ordered parameter declaration fact shapes
- [x] 1.2 Resolve each parameter's exact `I32`, unknown, or syntax-unavailable declared-type state
- [x] 1.3 Add function-local parameter lookup with resolved, missing, and ambiguous outcomes
- [x] 1.4 Emit deterministic `SEM0005` diagnostics for every later duplicate present parameter name

## 2. Resolve local references

- [x] 2.1 Add bare-identifier expression and reference fact shapes with exact syntax provenance
- [x] 2.2 Resolve returned and argument identifier expressions only against their enclosing function's complete parameter collection
- [x] 2.3 Propagate uniquely resolved parameter types into expression facts and return compatibility
- [x] 2.4 Emit `SEM0006` for present unknown references while preserving parser ownership and declaration-owned ambiguity diagnostics

## 3. Prove semantic isolation and determinism

- [x] 3.1 Add fixtures for resolved, unknown, duplicate, cross-function, unknown-type, and recovered parameters
- [x] 3.2 Test function-local identity, lookup, type propagation, diagnostic ownership, and stable ordering across repeated analysis
- [x] 3.3 Verify existing top-level function and call resolution remain unchanged

## 4. Make parameter relationships visible

- [x] 4.1 Add resolved, unknown, duplicate, cross-function, and recovered parameter-reference presets to the Syntax Inspector
- [x] 4.2 Render parameter cards and declaration-to-reference links with identities, spans, lookup state, types, and diagnostics
- [x] 4.3 Verify keyboard, screen-reader labeling, desktop layout, and narrow-viewport behavior in the live inspector

## 5. Document and verify

- [x] 5.1 Update compiler README, public API examples, changeset, and release-candidate validation for parameter facts and diagnostics
- [x] 5.2 Run focused compiler and docs tests, then `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`
