# Tasks

## 1. Typed Query Runtime

- [x] 1.1 Add the `SemanticQuery` session, closed header request/answer types, canonical keys, immutable observations, active reservations, and completed-answer store; verify focused query-runtime fixtures cover memoization, forced-fresh equivalence, nested memo-hit edges, cancellation cleanup, retry, and context-key separation.
- [x] 1.2 Add provider-owned semantic read helpers for headers, namespace membership, candidate sets, import selection, aliases, bounds, conformances, and profile/configuration facts; verify structural fixtures expose positive, negative, and candidate-set observations without post-hoc result scanning.

## 2. Header Providers

- [x] 2.1 Implement the authoritative declared-public-type provider with complete resolved/unavailable answer data, opaque-signature isolation, legal mutual header recursion, and explicit prohibited-cycle paths; verify declaration-index fixtures cover nominal, alias, inline-recursive, opaque, and cross-module signatures.
- [x] 2.2 Implement the authoritative name-resolution provider over immutable scope/index projections, preserving resolved, missing, conflicting, and unavailable outcomes; verify name-resolution fixtures cover visibility, changed imported bindings, missing-member additions, and candidate/conformance additions.

## 3. Compiler Integration

- [x] 3.1 Construct a fresh session for each header preparation/selection epoch, route declaration completion, final resolution, header conformance reads, and downstream header consumers through `Semantic.typeOf` and `Semantic.resolveName`, and verify identical source sets with changed selection cannot reuse stale answers.
- [x] 3.2 Seal the final semantic session into the prepared frontend without parser/source-resolver capability, remove superseded resolver/header dispatch and duplicate exports, and verify sealed semantic fixtures plus caller searches show one provider per migrated family.

## 4. Public Surface and Documentation

- [x] 4.1 Migrate root/subpath exports, `Semantic.typeOf` and `Semantic.resolveName` traces/reports/spans, compiler reference documentation, and the external architecture document's header-query input/output/dependency examples; verify generated or structural documentation checks observe only the new names and boundaries.
