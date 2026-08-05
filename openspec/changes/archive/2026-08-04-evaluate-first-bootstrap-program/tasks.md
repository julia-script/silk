## 1. Define bootstrap evaluation data

- [x] 1.1 Add the public BootstrapEvaluation actor with exact completed and closed blocked outcome shapes
- [x] 1.2 Add deterministic entry, call, binding, parameter-read, and return trace event shapes using existing identities and provenance
- [x] 1.3 Export the actor through the explicit compiler barrel and package subpath without compatibility aliases

## 2. Select and evaluate the entry path

- [x] 2.1 Select a unique zero-parameter `main` with resolved `I32` return type and return precise blocked entry reasons otherwise
- [x] 2.2 Evaluate available literal expressions to exact `I32` results
- [x] 2.3 Evaluate compatible calls left to right, create identity-keyed immutable parameter frames, and resolve parameter reads
- [x] 2.4 Stop at missing, ambiguous, incompatible, or unavailable reachable facts while retaining the successful trace prefix
- [x] 2.5 Ignore semantically broken declarations that are not reachable from the selected entry

## 3. Bound recursive evaluation

- [x] 3.1 Track the active declaration path and detect re-entry before evaluating a call target
- [x] 3.2 Return direct and mutual recursive cycles with ordered identities, closing call-site provenance, and partial traces
- [x] 3.3 Add deterministic tests proving valid results, positional binding, blocked states, unreachable isolation, and bounded cycles

## 4. Make execution visible

- [x] 4.1 Add an explicit browser-local Evaluate action and clear stale outcomes whenever source or preset input changes
- [x] 4.2 Render completed `I32` results, blocked reasons, and accessible provenance-linked trace events beside the static data-flow view
- [x] 4.3 Add literal, identity, two-parameter, missing-entry, wrong-arity, unavailable, direct-cycle, and mutual-cycle presets
- [x] 4.4 Verify completed and blocked evaluation interactively at desktop and narrow viewports without network requests, persistence, or UI hangs

## 5. Document, package, and verify

- [x] 5.1 Update compiler README to distinguish bootstrap evaluation from compilation, lowering, and a runtime
- [x] 5.2 Add a changeset and release-candidate validation for deep import, successful identity evaluation, trace order, and blocked cycles
- [x] 5.3 Run focused compiler and docs tests, then `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`
