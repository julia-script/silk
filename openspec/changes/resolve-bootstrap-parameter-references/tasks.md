## 1. Collect parameter declarations

- [ ] 1.1 Add source-local parameter identities and ordered parameter declaration fact shapes
- [ ] 1.2 Resolve each parameter's exact `I32`, unknown, or syntax-unavailable declared-type state
- [ ] 1.3 Add function-local parameter lookup with resolved, missing, and ambiguous outcomes
- [ ] 1.4 Emit deterministic `SEM0005` diagnostics for every later duplicate present parameter name

## 2. Resolve local references

- [ ] 2.1 Add bare-identifier expression and reference fact shapes with exact syntax provenance
- [ ] 2.2 Resolve returned and argument identifier expressions only against their enclosing function's complete parameter collection
- [ ] 2.3 Propagate uniquely resolved parameter types into expression facts and return compatibility
- [ ] 2.4 Emit `SEM0006` for present unknown references while preserving parser ownership and declaration-owned ambiguity diagnostics

## 3. Prove semantic isolation and determinism

- [ ] 3.1 Add fixtures for resolved, unknown, duplicate, cross-function, unknown-type, and recovered parameters
- [ ] 3.2 Test function-local identity, lookup, type propagation, diagnostic ownership, and stable ordering across repeated analysis
- [ ] 3.3 Verify existing top-level function and call resolution remain unchanged

## 4. Make parameter relationships visible

- [ ] 4.1 Add resolved, unknown, duplicate, cross-function, and recovered parameter-reference presets to the Syntax Inspector
- [ ] 4.2 Render parameter cards and declaration-to-reference links with identities, spans, lookup state, types, and diagnostics
- [ ] 4.3 Verify keyboard, screen-reader labeling, desktop layout, and narrow-viewport behavior in the live inspector

## 5. Document and verify

- [ ] 5.1 Update compiler README, public API examples, changeset, and release-candidate validation for parameter facts and diagnostics
- [ ] 5.2 Run focused compiler and docs tests, then `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`
