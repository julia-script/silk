## 1. Derive the visual flow model

- [ ] 1.1 Define browser-only flow node, edge, branch, terminal-state, and source-selection data
- [ ] 1.2 Project complete semantic facts into the ordered literal-to-parameter-to-reference-to-return path
- [ ] 1.3 Project arity mismatch, missing, ambiguous, and unavailable facts without inventing successful edges
- [ ] 1.4 Unit-test the projection for complete and incomplete programs independently of rendering

## 2. Render and navigate the path

- [ ] 2.1 Build the compact semantic flow lane with ordinary accessible document elements and lightweight connectors
- [ ] 2.2 Add the parallel ordered text relationship structure with identities, states, and owner-qualified spans
- [ ] 2.3 Add keyboard-operable node and edge selection that emphasizes matching source and existing detail cards
- [ ] 2.4 Render explicit unmatched, branched, and stopped states with labels that do not depend on color

## 3. Add visual feedback fixtures

- [ ] 3.1 Add complete identity, wrong-arity, unknown-reference, ambiguous-reference, and syntax-damaged presets
- [ ] 3.2 Keep preset, source, flow, and selection state disposable and restore the canonical unselected view on reload
- [ ] 3.3 Verify complete and incomplete paths manually in the live inspector at desktop and narrow viewports

## 4. Verify the docs surface

- [ ] 4.1 Add inspector component tests for accessible relationships, selection, recomputation, and absent-edge behavior
- [ ] 4.2 Update the hidden page explanation while keeping it absent from navigation, sidebars, and search
- [ ] 4.3 Run focused docs tests and build, then `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`
