## 1. Derive nested flow data

- [x] 1.1 Define a browser-side nested flow projection keyed by existing expression, call-site, argument, parameter, function, trace, and span identities
- [x] 1.2 Derive source-ordered call groups and inner-result-to-outer-argument relationships from semantic facts without reading relationships independently from concrete syntax
- [x] 1.3 Overlay only trace-backed reachable order, exact values, bindings, returns, and blocked endpoints after explicit evaluation
- [x] 1.4 Add projection tests for flat, nested, sibling, unavailable, wrong-arity, inner-blocked, and cyclic facts and outcomes

## 2. Render trustworthy nested flow

- [x] 2.1 Render bounded nested call lanes that preserve argument order, distinguish repeated callee call sites, and stack vertically at narrow widths
- [x] 2.2 Distinguish semantic relationships from evaluated values and order with explicit labels and a legend
- [x] 2.3 Terminate missing, ambiguous, incompatible, unavailable, blocked, and cyclic paths without drawing unperformed enclosing bindings or returns

## 3. Synchronize source and accessibility

- [x] 3.1 Connect every selectable group, node, edge, and terminal state to its exact source span and existing concrete, semantic, diagnostic, or trace detail
- [x] 3.2 Generate the accessible ordered flow structure from the same projection, including depth, ordinal, state, value, identity, and byte-range descriptions
- [x] 3.3 Add interaction and accessibility tests proving the visual and text structures expose the same relationships and selection behavior

## 4. Add visual feedback presets

- [x] 4.1 Add complete nested, nested sibling, inner unavailable, wrong-arity, inner-blocked, and nested-cycle presets while retaining the canonical flat preset
- [x] 4.2 Reset projection mode, evaluation overlay, selection, and source emphasis whenever input changes or the page reloads
- [x] 4.3 Manually verify all presets, source navigation, blocked endings, and static-versus-evaluated labeling at desktop and narrow widths

## 5. Document and verify

- [x] 5.1 Update the hidden inspector documentation to explain nested lanes, semantic versus evaluated flow, terminal states, and accessible navigation
- [x] 5.2 Run focused projection and docs tests, then `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`
