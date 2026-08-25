## 1. Existing Syntax and Tooling Slice

- [x] 1.1 Reserve `enum`, add lossless `EnumDeclaration` and `EnumMember` CST nodes, and parse default
      and explicit representation clauses plus optional signed decimal discriminants.
- [x] 1.2 Add declaration/member recovery boundaries and preserve exact source provenance.
- [x] 1.3 Add canonical formatter output for enum declarations and update CodeMirror, TextMate, and
      VS Code keyword highlighting.
- [x] 1.4 Verify the focused parser, formatter, and highlighting suites and compiler typecheck for the
      syntax slice (commit `84af799`).

## 2. Declaration and Discriminant Semantics

- [x] 2.1 Add canonical enum declaration and member facts to the declaration index, sharing the
      nominal type namespace, module visibility, deterministic ordering, and recovery model.
- [x] 2.2 Validate non-empty declarations, the fixed-width representation set, default `u8`, unique
      member names, and member-level visibility prohibition.
- [x] 2.3 Assign discriminants with checked host-independent integer arithmetic: first implicit `0`,
      then previous plus one across explicit and implicit members.
- [x] 2.4 Diagnose negative unsigned values, explicit range failures, implicit successor overflow,
      and duplicate discriminants with stable codes, exact spans, structured details, and related
      spans to first declarations.
- [x] 2.5 Add declaration-index and facade queries for representation, ordered members,
      discriminants, validity, and source provenance, including explicit unavailable recovery states.

## 3. Names, Types, Values, and Operators

- [x] 3.1 Resolve enum type names through ordinary module scopes and resolve `EnumName.Member` through
      the canonical enum identity, respecting enum visibility and diagnosing unknown or wrong-enum
      members.
- [x] 3.2 Type every member path as its nominal enum type and lower construction as a payload-free
      constant member value with no call or allocation.
- [x] 3.3 Add sealed `Intrinsic.enumValue` as the smallest target-neutral backing-value primitive and
      implement each declaration-generated `EnumName.value(value)` wrapper through it, returning the
      exact representation type; reject integer-to-enum conversion and enum/integer mixing.
- [x] 3.4 Implement `==` and `!=` for equal canonical enum types and reject cross-enum equality and
      direct enum ordering with dedicated diagnostics.
- [x] 3.5 Retain canonical enum and member identity in HIR and all analysis encodings rather than
      erasing enum values to untyped integers; verify no backend or standard-library declaration is
      recognized by the spelling `value`.

## 4. Matching

- [x] 4.1 Resolve qualified enum-member patterns against the scrutinee's canonical enum type without
      creating payload bindings or integer-pattern behavior.
- [x] 4.2 Extend coverage to the enum's closed member set; support complete member coverage and `_` as
      the universal remainder arm.
- [x] 4.3 Diagnose missing members, duplicate member arms, arms after `_`, foreign-enum members, and
      integer patterns with stable codes and exact member/arm spans.
- [x] 4.4 Preserve the enum source type through matching and keep result-type joins under existing
      match rules.

## 5. Ownership, Layout, and MIR

- [x] 5.1 Classify every scalar enum as sealed `Copy` with no cleanup obligation, independent of its
      representation and without user conformance.
- [x] 5.2 Plan exact representation size, alignment, and calling shape for every supported integer
      width, including one-member and default-`u8` enums, with no hidden metadata.
- [x] 5.3 Extend MIR logical types, constants, equality, conversion, and match decisions to retain
      canonical enum/member identity while carrying one verified scalar representation plan.
- [x] 5.4 Verify MIR rejects foreign members, wrong representation lanes, invalid discriminants, and
      incomplete enum match plans rather than relying on backends to repair them.

## 6. Execution Engines

- [x] 6.1 Evaluate enum construction, copying, `.value`, equality, inequality, and exhaustive matching
      by logical member identity.
- [x] 6.2 Lower verified enum values and operations to the selected integer lane in Wasm without
      independently choosing width or admitting arbitrary integers.
- [x] 6.3 Lower the same verified values and operations in the native LLVM backend with exact ABI
      width/alignment and no hidden metadata.
- [x] 6.4 Add enum programs to the differential acceptance corpus so evaluator, Wasm, and native
      results agree without per-feature redundant native tests.

## 7. Diagnostics, Tooling, and Documentation

- [x] 7.1 Add every enum diagnostic to the generated catalog and test codes, primary spans, related
      spans, and structured payloads rather than wording.
- [x] 7.2 Expose enum declarations, members, references, types, HIR/MIR facts, layouts, match coverage,
      and emission provenance through immutable analysis-facade queries.
- [x] 7.3 Update hover, navigation, labs, syntax snapshots, and generated artifacts that consume the
      compiler token or declaration inventories; tooling must not reconstruct enum semantics.
- [x] 7.4 Document default `u8`, explicit widening, member construction, `.value`, nominal equality,
      exhaustive matching, and the distinction from structural unions.

## 8. Acceptance and Repository Gates

- [x] 8.1 Accept default and explicit signed/unsigned enums at boundary widths, including mixed
      explicit/implicit discriminants and a one-member enum.
- [x] 8.2 Reject empty declarations, unsupported representations, duplicate names/discriminants,
      signedness errors, explicit overflow, and implicit overflow while preserving unrelated facts.
- [x] 8.3 Accept same-enum equality and exhaustive matching; reject cross-enum/integer operations,
      ordering, foreign/integer patterns, and incomplete or unreachable arms.
- [x] 8.4 Prove exact layout and calling shape and cross-engine observable parity at the cheapest test
      tier appropriate to each claim.
- [x] 8.5 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`; report exact
      failures and whether they predate this change.
- [x] 8.6 Run `pnpm release:candidate` because compiler package contents, generated artifacts, or
      exports change.
