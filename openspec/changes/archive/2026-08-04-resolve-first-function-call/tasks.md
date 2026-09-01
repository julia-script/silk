## 1. Dependency and Reference Model

- [x] 1.1 Confirm `parse-first-function-call` is synced and archived and the canonical specs contain
      concrete call syntax and the unresolved call-expression fact.
- [x] 1.2 Replace the temporary unresolved call state with immutable resolved, missing, ambiguous,
      and syntax-unavailable reference states carrying exact call and target provenance.
- [x] 1.3 Add immutable `SEM0004` unknown-function diagnostics with exact callee spans and reason data
      while keeping ambiguity owned by existing `SEM0003` declaration diagnostics.

## 2. Two-Pass Resolution

- [x] 2.1 Separate complete declaration-header/name grouping from returned-expression analysis so
      backward, forward, and self references use the same lookup result.
- [x] 2.2 Resolve unique targets, retain all ambiguous target identities in source order, preserve
      missing/unavailable states, and never select the first duplicate.
- [x] 2.3 Propagate a resolved target's declared return type to the call expression and compute caller
      compatibility independently from target-body compatibility.
- [x] 2.4 Merge and order unknown-name, duplicate-name, type, and integer semantic diagnostics without
      crossing lexical or parser diagnostic ownership.
- [x] 2.5 Add fixtures and tests for backward, forward, self, unknown, ambiguous, damaged, and
      unresolved-target-type calls plus repeated deterministic analysis.

## 3. Public Compiler Boundary

- [x] 3.1 Update compiler README examples and API documentation with the first resolved relationship,
      call type propagation, and explicit no-execution/no-HIR boundary.
- [x] 3.2 Add a Changesets entry and extend packed root/deep release validation for reference states
      and `SEM0004` without source files or undeclared runtime dependencies.

## 4. Resolution Inspector

- [x] 4.1 Add resolved-backward, resolved-forward, self, unknown, and ambiguous call presets.
- [x] 4.2 Render compact accessible caller-to-target relations with caller, call-site, and target
      identities/spans plus missing and ambiguous alternatives.
- [x] 4.3 Browser-test every relation state, type propagation, compatibility, diagnostics, responsive
      layout, and continued hidden-route/search behavior in the production build.

## 5. Verification

- [x] 5.1 Run strict OpenSpec validation and focused compiler/docs typecheck, test, format,
      production-build, and browser-smoke commands.
- [x] 5.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and
      `pnpm release:candidate`, fixing every introduced failure.
