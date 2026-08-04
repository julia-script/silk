## 1. Dependency and Semantic Model

- [ ] 1.1 Confirm `parse-multiple-bootstrap-functions` is synced and archived and the canonical
  syntax spec contains the non-empty declaration sequence and boundary-recovery requirements.
- [ ] 1.2 Replace singular result fields with an immutable ordered `FunctionFact` collection that
  groups each declaration, returned integer fact, and compatibility without compatibility aliases.
- [ ] 1.3 Generalize declaration identities to deterministic non-negative source-order ordinals and
  preserve exact function/name/type/body syntax provenance.
- [ ] 1.4 Add closed resolved, missing, and ambiguous declaration-name lookup outcomes in data-first
  and pipeable forms.
- [ ] 1.5 Add immutable `SEM0003` duplicate-name diagnostics with later-name spans and precise reason
  data while excluding recovered missing names.

## 2. Declaration Collection

- [ ] 2.1 Collect every direct function declaration in source order and independently analyze its
  return type, integer expression, and compatibility using the existing bounded traversal.
- [ ] 2.2 Derive lookup groups and duplicate diagnostics from the same present-name collection and
  sort all semantic diagnostics deterministically by span and code.
- [ ] 2.3 Add readable fixtures for two and three functions, missing names, duplicate names, mixed
  return-type/integer damage, and parser-plus-semantic damage.
- [ ] 2.4 Test ordinals, frozen ownership, unique/missing/ambiguous lookup, every later duplicate,
  per-function isolation, diagnostic phase ownership, and repeated determinism.

## 3. Public Compiler Boundary

- [ ] 3.1 Update the compiler README and public type documentation for ordered function facts and
  closed lookup outcomes, explicitly deferring calls and scope graphs.
- [ ] 3.2 Add a Changesets entry and extend release-candidate validation to exercise the breaking
  result shape through root and deep imports from a packed offline consumer.

## 4. Declaration Inspector

- [ ] 4.1 Replace the first-only notice with ordered semantic function cards showing identity, name,
  return type, integer value, compatibility, and provenance for every declaration.
- [ ] 4.2 Add duplicate-name and mixed-damage presets plus an accessible lookup-state summary.
- [ ] 4.3 Browser-test one, two, and three functions, missing names, duplicate names, mixed damage,
  responsive card layout, and continued hidden-route/search behavior in the production build.

## 5. Verification

- [ ] 5.1 Run strict OpenSpec validation and focused compiler/docs typecheck, test, format,
  production-build, and browser-smoke commands.
- [ ] 5.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and
  `pnpm release:candidate`, fixing every introduced failure.
