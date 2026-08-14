## 1. Authoring Policy and Public-Surface Inventory

- [x] 1.1 Add the Silk stdlib documentation guide with the required summary, ordered optional sections, declaration-owned parameter rules, semantic-link conventions, example quality rules, and evidence checklist from the design.
- [x] 1.2 Generate a source-located inventory of every manifest module, public root declaration, public field, service or interface operation, parameter, type parameter, implementation, and implementation operation, including current documentation coverage.
- [x] 1.3 Classify questionable public representation-state declarations as user-facing, structurally exposed, or likely visibility defects; record likely defects in the local Markdown tracker without changing API visibility in this change.
- [x] 1.4 Add focused documentation-policy fixtures covering complete and missing coverage, concise summaries, ordered and omitted sections, parameter ownership, titled examples, fence attributes, and semantic links.

## 2. Documentation Policy Checker

- [x] 2.1 Implement the stdlib documentation policy checker over compiler analysis and the shared documentation project without adding diagnostics to ordinary compiler analysis.
- [x] 2.2 Report each policy violation with the canonical declaration identity and source location, including coverage, section shape, parameter placement, example title or placement, fence attributes, and unresolved stdlib link failures.
- [x] 2.3 Add checker tests proving private helpers are excluded, obvious undocumented operands are allowed, required public surfaces are enforced, and ordinary Silk compilation remains independent of policy results.
- [x] 2.4 Add a package-local command that runs the policy checker against every module in the canonical stdlib manifest, but defer adding it to the root verification path until the source pass is green.

## 3. Structured Markdown Reference

- [x] 3.1 Implement a structured Markdown renderer for documentation blocks that rebases headings, preserves code fences and lists, renders resolved symbol links to generated targets, and leaves unresolved links readable.
- [x] 3.2 Implement recursive item rendering for every documentation-model kind, honoring model visibility and source order for root declarations, type parameters, fields, parameters, service or interface operations, implementations, and implementation operations.
- [x] 3.3 Derive declaration anchors and cross-page link destinations from canonical identities and reject deterministic path or anchor collisions.
- [x] 3.4 Add renderer tests for nested public items, hidden private items, accurate public counts, heading rebasing, multiple titled examples, resolved and unresolved links, and byte-identical repeated output.
- [x] 3.5 Replace monolithic stdlib generation with `packages/language/docs/stdlib/README.md` plus one deterministic page per manifest module, preserving the `/docs/language/stdlib` landing route.
- [x] 3.6 Update repository documentation links for the module-page layout and remove the obsolete generated `packages/language/docs/stdlib.md` in the same change.
- [x] 3.7 Extend documentation freshness checking to detect missing, extra, stale, renamed, and byte-different generated stdlib pages.

## 4. Example Verification

- [x] 4.1 Add focused doctest coverage for titled stdlib examples, complete-module compilation, `silk,ignore` reporting, malformed attributes, and source-located failures.
- [x] 4.2 Add a package-local command that compiles every executable stdlib documentation example and reports collected, passed, skipped, and failed counts.
- [x] 4.3 Cross-reference each new example's runtime, ownership, lifecycle, failure, or ordering claim with an existing behavioral test or add the narrow missing behavioral assertion before publishing the claim.

## 5. Foundational Value and Substrate Documentation

For each module task below, inspect implementation, tests, call sites, related APIs, maintained
design prose, and relevant archived specs; then add module documentation, complete required public
coverage, add only valuable local parameter or type-parameter docs, concentrate examples on
semantic anchors, add useful semantic links, and pass scoped policy, doctest, and generation checks.

- [x] 5.1 Complete the `silk/option` documentation pass.
- [x] 5.2 Complete the `silk/result` documentation pass.
- [x] 5.3 Complete the `silk/box` documentation pass.
- [x] 5.4 Complete the `silk/bytes` documentation pass.
- [x] 5.5 Complete the `silk/string` documentation pass.
- [x] 5.6 Complete the `silk/layout` documentation pass.
- [x] 5.7 Complete the `silk/raw-buffer` documentation pass, clearly separating unsafe caller proofs from checked APIs.
- [x] 5.8 Complete the `silk/slot` documentation pass, clearly separating unsafe caller proofs from checked APIs.
- [x] 5.9 Complete the `silk/order` documentation pass.
- [x] 5.10 Complete the `silk/format` documentation pass.

## 6. Collection Documentation

Apply the evidence and verification checklist from Section 5 to each collection module.

- [x] 6.1 Complete the `silk/vector` documentation pass, including allocation, capacity, ownership, search, and sorting contracts.
- [x] 6.2 Complete the `silk/hash` documentation pass, including seed, determinism, and witness-selection contracts.
- [x] 6.3 Complete the `silk/hash_map` documentation pass, including iteration order, probing, replacement, removal, ownership, and allocation behavior.
- [x] 6.4 Complete the `silk/hash_set` documentation pass, including iteration order, membership, replacement, removal, ownership, and allocation behavior.

## 7. Effect and Portable Service Documentation

Apply the evidence and verification checklist from Section 5 to each Effect or portable-service
module, including stable failure, requirement, lifecycle, provider, and portability behavior.

- [x] 7.1 Complete the `silk/core` documentation pass.
- [x] 7.2 Complete the `silk/effects` documentation pass.
- [x] 7.3 Complete the `silk/logging` documentation pass.
- [x] 7.4 Complete the `silk/metrics` documentation pass.
- [x] 7.5 Complete the `silk/filesystem` documentation pass.
- [x] 7.6 Complete the `silk/child_process` documentation pass.
- [x] 7.7 Complete the `silk/host_input` documentation pass.
- [x] 7.8 Complete the `silk/standard_input` documentation pass.

## 8. Native Provider Documentation

Apply the evidence and verification checklist from Section 5 to each native provider, clearly
separating portable service contracts from platform-specific implementation and availability.

- [x] 8.1 Complete the `silk/os_child_process` documentation pass.
- [x] 8.2 Complete the `silk/os_filesystem` documentation pass.
- [x] 8.3 Complete the `silk/os_host_input` documentation pass.
- [x] 8.4 Complete the `silk/os_standard_input` documentation pass.

## 9. Numeric and Primitive Family Documentation

Apply the evidence and verification checklist from Section 5 while keeping repeated families
consistent and concentrating examples on checked, wrapping, saturating, conversion, parsing,
formatting, comparison, and floating-point boundary semantics rather than duplicating calls.

- [x] 9.1 Complete the shared `silk/numeric` documentation pass.
- [x] 9.2 Complete the `silk/bool` documentation pass.
- [x] 9.3 Complete the `silk/char` documentation pass.
- [x] 9.4 Complete the `silk/i8` documentation pass.
- [x] 9.5 Complete the `silk/i16` documentation pass.
- [x] 9.6 Complete the `silk/i32` documentation pass.
- [x] 9.7 Complete the `silk/i64` documentation pass.
- [x] 9.8 Complete the `silk/isize` documentation pass.
- [x] 9.9 Complete the `silk/u8` documentation pass.
- [x] 9.10 Complete the `silk/u16` documentation pass.
- [x] 9.11 Complete the `silk/u32` documentation pass.
- [x] 9.12 Complete the `silk/u64` documentation pass.
- [x] 9.13 Complete the `silk/usize` documentation pass.
- [x] 9.14 Complete the `silk/f32` documentation pass.
- [x] 9.15 Complete the `silk/f64` documentation pass.
- [x] 9.16 Audit repeated numeric-family summaries, sections, links, and example selection for semantic consistency without erasing type-specific boundaries.

## 10. Integration and Final Verification

- [x] 10.1 Run the public-surface inventory again and confirm required module, root declaration, public field, and service or interface operation coverage is complete with no undocumented accidental omissions.
- [x] 10.2 Audit every existing and new stdlib example, deliberately keeping, fixing, replacing, or removing it; confirm every remaining ignored example is justified and every executable example compiles.
- [x] 10.3 Audit cross-module semantic links, generated anchors, module summaries, public declaration counts, source order, and documentation-site navigation across all 41 modules.
- [x] 10.4 Wire the now-green stdlib policy command, doctest command, and generated-output freshness check into the normal compiler package verification reached by root `pnpm check`.
- [x] 10.5 Regenerate the full documentation tree and verify a second fresh generation produces no diff.
- [x] 10.6 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test`, fixing documentation-tooling or generated-output failures without weakening the policy.
- [x] 10.7 Run `pnpm check`, build the documentation application, and inspect representative module pages for hierarchy, Markdown rendering, code highlighting, links, and navigation.
- [x] 10.8 Run `pnpm release:candidate` because shipped package documentation contents change, and report any failure with whether it predates this change.
- [x] 10.9 Confirm the final diff contains no unintended runtime, compiler-semantic, public-type, or visibility change and report all separately tracked public-surface concerns.
