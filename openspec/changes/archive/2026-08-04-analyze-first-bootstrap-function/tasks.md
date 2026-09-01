## 1. Semantic Fact Model

- [x] 1.1 Implement the `SemanticDiagnostic` actor with immutable `SEM0001` unknown-type and
      `SEM0002` integer-range diagnostics, precise reason data, concise messages, and source-owned spans.
- [x] 1.2 Define the closed `SemanticAnalysis` result, declaration identity and fact, declared-name,
      return-type, integer-expression, and return-compatibility states without nullable fields or a
      generic fact framework.
- [x] 1.3 Add immutable declaration-name lookup and tests for same-source identity data, distinct
      source identities, frozen result data, present names, and unavailable names.

## 2. First Function Analysis

- [x] 2.1 Add bounded node-kind and token-kind traversal that locates the function declaration,
      declared name, return-type reference, and integer expression without child-index assumptions or
      descent into error regions.
- [x] 2.2 Analyze the function declaration into public visibility, zero parameters, deterministic
      source-local identity, original syntax provenance, and a present or unavailable name fact.
- [x] 2.3 Resolve only the exact ASCII spelling `I32`, preserve unknown and unavailable type states,
      and emit `SEM0001` only for a present unknown identifier.
- [x] 2.4 Interpret decimal token bytes exactly, publish safe in-range `I32` values, preserve
      out-of-range and unavailable states, and emit `SEM0002` only for a present overflowing literal.
- [x] 2.5 Compute `Compatible` or `Unavailable` return compatibility, retain the exact parse result,
      and sort readonly semantic diagnostics deterministically without merging diagnostic phases.
- [x] 2.6 Add readable semantic fixtures for the accepted source, missing name, unknown type,
      damaged type, positive `I32` boundary, overflow, missing integer, mixed parser and semantic damage,
      and values beyond host safe-integer precision.
- [x] 2.7 Test exact facts and provenance, lookup, phase-owned diagnostics, boundary arithmetic,
      unavailable propagation, repeated determinism, and total completion for every fixture.

## 3. Public Compiler Boundary

- [x] 3.1 Export only the new `SemanticAnalysis` and `SemanticDiagnostic` namespaces from the
      compiler root and add their explicit package subpaths.
- [x] 3.2 Extend the compiler README with the semantic-analysis example, fact-state and diagnostic
      ownership contract, exact `I32` boundary, and explicit boundary before AST, HIR, and MIR.
- [x] 3.3 Add a Changesets entry and extend release-candidate validation to pack and import both new
      root and deep exports without source files or undeclared runtime dependencies.

## 4. Hidden Semantic Inspector

- [x] 4.1 Run semantic analysis in the existing client-only Syntax Inspector and add unknown-type
      and out-of-range example presets while preserving the accepted source on initial load.
- [x] 4.2 Render an accessible semantic-facts section for declaration identity/name, declared type,
      integer type/value, return compatibility, provenance spans, and semantic diagnostics without
      obscuring the concrete tree or the lexer and parser diagnostic collections.
- [x] 4.3 Browser-test valid, empty, missing-syntax, unexpected punctuation, unknown-type, overflow,
      and unsupported-Unicode states in the production build; confirm the route remains absent from
      navigation, content search, and published compiler files.

## 5. Verification

- [x] 5.1 Run strict OpenSpec validation and the focused compiler and docs typecheck, test, format,
      production-build, and browser-smoke commands.
- [x] 5.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and
      `pnpm release:candidate`, fixing every introduced failure and reporting any proven pre-existing
      failure exactly.
