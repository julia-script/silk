## 1. Add lossless aggregate syntax

- [x] 1.1 Reserve `tuple` and add syntax kinds for named tuple declarations, positional tuple
  literals, contextual record literals, and ordinal projections; verify lexer and syntax encoding
  tests distinguish them from identifiers, calls, grouping, unit, blocks, and named struct literals.
- [x] 1.2 Parse `tuple Point(T0, T1)`, `Point(v0, v1)`, `(v0, v1)`, `(v0,)`, `.{ field: value }`,
  and `.0` with optional trailing commas and exact trivia; verify parser round-trip tests cover every
  accepted form and keep `(value)` as grouping and `()` as unit.
- [x] 1.3 Add bounded recovery for missing tuple names, types, elements, record labels, colons,
  values, commas, and delimiters, and reject labeled tuples; verify damaged-form fixtures preserve
  the following member, statement, and declaration without a diagnostic cascade.
- [x] 1.4 Extend the formatter and syntax inspector for the new source forms without exposing
  synthesized member names; verify formatting is idempotent and syntax-inspector goldens retain
  exact spans and source order.

## 2. Generalize nominal aggregate identities and declarations

- [x] 2.1 Add an `AggregateIdentity` actor for source and occurrence-generated nominal identities,
  deriving anonymous identities only from canonical module, syntax occurrence, and aggregate kind;
  verify equal source is deterministic across the existing fresh-process canary and distinct
  literal occurrences never collide.
- [x] 2.2 Generalize canonical aggregate members to a closed labeled-or-ordinal identity while
  preserving existing struct field behavior; verify unit tests encode labels and ordinals
  deterministically and no `_0`-style source spelling enters ordinal lookup.
- [x] 2.3 Index named tuple declarations as positional nominal aggregates and record generated
  anonymous declarations in semantic aggregate facts without inserting them into lexical lookup,
  imports, or exports; verify declaration, module-surface, completion, and navigation tests enforce
  those boundaries.
- [x] 2.4 Add diagnostic catalog entries for tuple arity and position mismatches, incompatible
  contextual aggregate kinds, uncontextualized branch disagreement, and forbidden synthetic-field
  construction; regenerate the catalog and verify tests assert codes, semantic details, and spans.

## 3. Implement contextual and anonymous aggregate analysis

- [x] 3.1 Add the narrow optional-expected-aggregate analysis path at explicit bindings, declared
  returns, known call parameters, and other independently determined contracts; verify negative
  tests show that later uses and shape matching do not participate in expected-type discovery.
- [x] 3.2 Resolve named tuple construction, contextual tuple literals, and ordinal projections with
  exact arity, element compatibility, left-to-right evaluation, and nominal identity; verify
  `Point(0, 0)`, `let origin: Point = (0, 0)`, `.0`, and invalid named-field construction.
- [x] 3.3 Resolve contextual `.{ ... }` literals through the existing struct authority,
  visibility, completeness, generic inference, and canonical field-mapping rules without a type-name
  lookup at the call site; verify public construction, reordered fields, private fields, opaque
  types, duplicates, missing fields, and conflicting generic constraints.
- [x] 3.4 Finalize each uncontextualized tuple or record literal once as an occurrence-nominal
  declaration with inferred ordered members; verify a local binding preserves that exact type
  through projections, borrows, and generic calls while no visible same-shaped declaration is
  selected.
- [x] 3.5 Enforce nominal boundaries for anonymous aggregates; verify separate same-shaped literals
  fail assignment, equality, and uncontextualized branch joining, while an explicit enclosing named
  type context makes both branches construct the same nominal type. Update the exhaustive-match
  capability so its ordinary nominal-union join explicitly excludes distinct anonymous occurrences.

## 4. Erase aggregate sugar into the existing runtime pipeline

- [x] 4.1 Elaborate every accepted named tuple, contextual literal, and anonymous aggregate into
  canonical nominal struct construction or projection HIR with source evaluation provenance and
  canonical member order; verify HIR goldens contain no tuple-specific or structural-record runtime
  node.
- [x] 4.2 Carry occurrence-generated nominal identities through generic substitution and runtime
  instance reachability; verify repeated uses of one anonymous binding share a specialization,
  separate occurrences stay distinct, and unreachable generated declarations remain semantic-only.
- [x] 4.3 Apply ordinary struct Copy evidence, moves, borrows, partial-move rejection, and recursive
  cleanup to named and anonymous aggregates; verify anonymous all-Copy records remain affine,
  whole-value moves transfer one obligation, and ordinal cleanup order is exact.
- [x] 4.4 Confirm layout, MIR, evaluator, LLVM, and WebAssembly consume only the generalized nominal
  aggregate representation; verify the existing cross-engine aggregate corpus covers named tuples
  and anonymous records without introducing backend-specific tuple categories or support paths.

## 5. Publish facts, documentation, and acceptance evidence

- [x] 5.1 Extend semantic facts, deterministic encodings, hover, navigation, and contextual field
  completion with expected targets, occurrence identities, member mappings, and source provenance;
  verify anonymous aggregates are presented without fabricated importable names.
- [x] 5.2 Update the prescriptive language reference for tuple declarations, positional literals and
  projections, contextual record literals, anonymous nominality, inference boundaries, ownership,
  and rejected labeled or structural forms; verify every documentation example parses.
- [x] 5.3 Add shared `Analysis.evaluate` fixtures for function-call convenience, locally bound generic
  argument bundles, explicit branch context, nominal failures, and deterministic facts, adding
  backend legs only for claims about representation or code generation.
- [x] 5.4 Regenerate affected compiler and documentation artifacts, then run `pnpm typecheck`,
  `pnpm exec biome check .`, `pnpm test`, and `pnpm check` in order; because package contents change,
  also run `pnpm release:candidate` and report any exact failure and whether it predates this change.
