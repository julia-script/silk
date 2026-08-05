## 1. Lossless import and visibility syntax

- [x] 1.1 Add the `as` keyword and concrete import-path, namespace-alias, selected-member-list, and member-entry node vocabulary, including stable syntax encoding coverage.
- [x] 1.2 Parse namespace, changed-alias, selective, member-alias, and hybrid import forms with exact trivia/token retention and focused accepted parser tests.
- [x] 1.3 Add bounded recovery for missing import path segments, aliases, members, commas, and closing braces, with parser diagnostics and following-declaration preservation tests.
- [x] 1.4 Make `pub` optional on function declarations, preserve its concrete presence or absence, and add public/private syntax and recovery fixtures without changing existing public programs.

## 2. Canonical module paths and closure facts

- [x] 2.1 Validate compilation-request module identities as exact case-sensitive, extensionless, slash-separated logical paths and reject malformed caller input before parsing.
- [x] 2.2 Derive canonical slash identities from complete dotted import paths, retain the full concrete path and binding clause in import facts, and keep damaged paths unavailable without module-diagnostic cascades.
- [x] 2.3 Extend module-closure tests for multi-segment paths, exact-case mismatch, rename identity changes, self/unknown imports, diamonds, cycles, supply-order independence, and binding-clause-independent loading.

## 3. Closure-wide headers and name resolution

- [x] 3.1 Extend declaration headers with public/default-private visibility, index both kinds once across the closure, and migrate header/index tests and consumers to the published authority.
- [x] 3.2 Add `MOD0003` and `SEM0013`–`SEM0016` structured diagnostics for repeated target imports, redundant aliases, unknown imported members, inaccessible members, and flat-scope conflicts, including cause and no-cascade tests.
- [x] 3.3 Add the public `NameResolution` actor and package export with immutable resolution, module-scope, import, binding, conflict, and closed lookup data models.
- [x] 3.4 Build module scopes in canonical order from intrinsic actors, indexed local declarations, namespace imports, and selected members; enforce one import per target, changed aliases, and public-member visibility.
- [x] 3.5 Detect every same-spelling binding collision without choosing a declaration-kind or source-order winner, including conflicts with `I32` and `Bool`, and retain every candidate and diagnostic cause.
- [x] 3.6 Implement unqualified and namespace-qualified lookup operations and verify namespace, selective, hybrid, private, missing, conflicting, damaged, cyclic, and fresh-process deterministic outcomes.

## 4. Closure-aware elaboration and facade

- [x] 4.1 Replace phase-local header recollection with an elaboration input containing the module's published headers and completed scope; remove superseded unreleased entry points and migrate unit fixtures directly.
- [x] 4.2 Resolve local/private, selectively imported, and namespace-qualified calls through `NameResolution`, route intrinsic actor bindings to existing builtin calls, and suppress secondary unknown-call diagnostics after caused failures.
- [x] 4.3 Update HIR facts, encoders, and goldens so imported calls use ordinary canonical call operations and every unavailable lookup retains its originating diagnostic identity.
- [x] 4.4 Store the resolution artifact in `Analysis.Snapshot`, merge its diagnostics in deterministic phase order, and add facade queries for scopes, import bindings, conflicts, and closed lookups.
- [x] 4.5 Extend analysis and facade-boundary tests to prove HIR, ownership, instance, layout, MIR, evaluation, and codegen queries consume the same cross-module snapshot facts without tooling reconstructing scopes.

## 5. Cross-module runtime reachability

- [x] 5.1 Extend instance discovery tests for selected and namespace-qualified call chains, unreachable imported declarations, diamond sharing, and cross-module mutual recursion keyed by canonical declaration identity.
- [x] 5.2 Add a multi-module corpus that lowers, interprets, and emits LLVM for private local helpers and public calls spanning at least three modules, including native differential parity.
- [x] 5.3 Run the same supported cross-module corpus through the WebAssembly backend and verify canonical target/layout reuse, deterministic output, and interpreter parity without a module-specific backend path.
- [x] 5.4 Add driver and deterministic encoding coverage proving source supply order and import traversal order do not change HIR, MIR, symbols, diagnostics, or executable behavior.

## 6. Facade-only inspection

- [x] 6.1 Update the declaration-index lab to display public/private visibility and cross-module importability with focused component tests.
- [x] 6.2 Add the direct-link name-resolution lab and presets for namespace, selective, hybrid, aliases, private/unknown members, collisions, damaged imports, and valid cycles.
- [x] 6.3 Link name-resolution entries to call-site facts, indexed headers, HIR targets, and discovered instances by canonical identity, with accessible text and exact source provenance.
- [x] 6.4 Update module-closure, HIR, instance-discovery, and pipeline views as needed and extend the automated facade-only boundary check to reject direct phase imports from the new lab.

## 7. Verification and handoff

- [x] 7.1 Run targeted compiler and docs suites while implementing, refresh all affected syntax/HIR/MIR/IR/binary goldens and digests intentionally, and keep the existing single-module corpus passing.
- [x] 7.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`, reporting any pre-existing diagnostics separately.
- [x] 7.3 Run `pnpm release:candidate` because compiler exports and packed contents change, then run strict OpenSpec validation and review every scenario against retained test evidence.
