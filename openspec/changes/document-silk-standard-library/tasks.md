## 1. Authoring Policy and Public-Surface Inventory

- [ ] 1.1 Add the Silk stdlib documentation guide with the required summary, ordered optional sections, declaration-owned parameter rules, semantic-link conventions, example quality rules, and evidence checklist from the design.
- [ ] 1.2 Generate a source-located inventory of every manifest module, public root declaration, public field, service or interface operation, parameter, type parameter, implementation, and implementation operation, including current documentation coverage.
- [ ] 1.3 Classify questionable public representation-state declarations as user-facing, structurally exposed, or likely visibility defects; record likely defects in the local Markdown tracker without changing API visibility in this change.
- [ ] 1.4 Add focused documentation-policy fixtures covering complete and missing coverage, concise summaries, ordered and omitted sections, parameter ownership, titled examples, fence attributes, and semantic links.

## 2. Documentation Policy Checker

- [ ] 2.1 Implement the stdlib documentation policy checker over compiler analysis and the shared documentation project without adding diagnostics to ordinary compiler analysis.
- [ ] 2.2 Report each policy violation with the canonical declaration identity and source location, including coverage, section shape, parameter placement, example title or placement, fence attributes, and unresolved stdlib link failures.
- [ ] 2.3 Add checker tests proving private helpers are excluded, obvious undocumented operands are allowed, required public surfaces are enforced, and ordinary Silk compilation remains independent of policy results.
- [ ] 2.4 Add a package-local command that runs the policy checker against every module in the canonical stdlib manifest, but defer adding it to the root verification path until the source pass is green.

## 3. Structured Markdown Reference

- [ ] 3.1 Implement a structured Markdown renderer for documentation blocks that rebases headings, preserves code fences and lists, renders resolved symbol links to generated targets, and leaves unresolved links readable.
- [ ] 3.2 Implement recursive item rendering for every documentation-model kind, honoring model visibility and source order for root declarations, type parameters, fields, parameters, service or interface operations, implementations, and implementation operations.
- [ ] 3.3 Derive declaration anchors and cross-page link destinations from canonical identities and reject deterministic path or anchor collisions.
- [ ] 3.4 Add renderer tests for nested public items, hidden private items, accurate public counts, heading rebasing, multiple titled examples, resolved and unresolved links, and byte-identical repeated output.
- [ ] 3.5 Replace monolithic stdlib generation with `packages/language/docs/stdlib/README.md` plus one deterministic page per manifest module, preserving the `/docs/language/stdlib` landing route.
- [ ] 3.6 Update repository documentation links for the module-page layout and remove the obsolete generated `packages/language/docs/stdlib.md` in the same change.
- [ ] 3.7 Extend documentation freshness checking to detect missing, extra, stale, renamed, and byte-different generated stdlib pages.

## 4. Example Verification

- [ ] 4.1 Add focused doctest coverage for titled stdlib examples, complete-module compilation, `silk,ignore` reporting, malformed attributes, and source-located failures.
- [ ] 4.2 Add a package-local command that compiles every executable stdlib documentation example and reports collected, passed, skipped, and failed counts.
- [ ] 4.3 Cross-reference each new example's runtime, ownership, lifecycle, failure, or ordering claim with an existing behavioral test or add the narrow missing behavioral assertion before publishing the claim.

## 5. Foundational Value and Substrate Documentation

For each module task below, inspect implementation, tests, call sites, related APIs, maintained
design prose, and relevant archived specs; then add module documentation, complete required public
coverage, add only valuable local parameter or type-parameter docs, concentrate examples on
semantic anchors, add useful semantic links, and pass scoped policy, doctest, and generation checks.

- [ ] 5.1 Complete the `silk/option` documentation pass.
- [ ] 5.2 Complete the `silk/result` documentation pass.
- [ ] 5.3 Complete the `silk/box` documentation pass.
- [ ] 5.4 Complete the `silk/bytes` documentation pass.
- [ ] 5.5 Complete the `silk/string` documentation pass.
- [ ] 5.6 Complete the `silk/layout` documentation pass.
- [ ] 5.7 Complete the `silk/raw-buffer` documentation pass, clearly separating unsafe caller proofs from checked APIs.
- [ ] 5.8 Complete the `silk/slot` documentation pass, clearly separating unsafe caller proofs from checked APIs.
- [ ] 5.9 Complete the `silk/order` documentation pass.
- [ ] 5.10 Complete the `silk/format` documentation pass.

## 6. Collection Documentation

Apply the evidence and verification checklist from Section 5 to each collection module.

- [ ] 6.1 Complete the `silk/vector` documentation pass, including allocation, capacity, ownership, search, and sorting contracts.
- [ ] 6.2 Complete the `silk/hash` documentation pass, including seed, determinism, and witness-selection contracts.
- [ ] 6.3 Complete the `silk/hash_map` documentation pass, including iteration order, probing, replacement, removal, ownership, and allocation behavior.
- [ ] 6.4 Complete the `silk/hash_set` documentation pass, including iteration order, membership, replacement, removal, ownership, and allocation behavior.

## 7. Effect and Portable Service Documentation

Apply the evidence and verification checklist from Section 5 to each Effect or portable-service
module, including stable failure, requirement, lifecycle, provider, and portability behavior.

- [ ] 7.1 Complete the `silk/core` documentation pass.
- [ ] 7.2 Complete the `silk/effects` documentation pass.
- [ ] 7.3 Complete the `silk/logging` documentation pass.
- [ ] 7.4 Complete the `silk/metrics` documentation pass.
- [ ] 7.5 Complete the `silk/filesystem` documentation pass.
- [ ] 7.6 Complete the `silk/child_process` documentation pass.
- [ ] 7.7 Complete the `silk/host_input` documentation pass.
- [ ] 7.8 Complete the `silk/standard_input` documentation pass.

## 8. Native Provider Documentation

Apply the evidence and verification checklist from Section 5 to each native provider, clearly
separating portable service contracts from platform-specific implementation and availability.

- [ ] 8.1 Complete the `silk/os_child_process` documentation pass.
- [ ] 8.2 Complete the `silk/os_filesystem` documentation pass.
- [ ] 8.3 Complete the `silk/os_host_input` documentation pass.
- [ ] 8.4 Complete the `silk/os_standard_input` documentation pass.

## 9. Numeric and Primitive Family Documentation

Apply the evidence and verification checklist from Section 5 while keeping repeated families
consistent and concentrating examples on checked, wrapping, saturating, conversion, parsing,
formatting, comparison, and floating-point boundary semantics rather than duplicating calls.

- [ ] 9.1 Complete the shared `silk/numeric` documentation pass.
- [ ] 9.2 Complete the `silk/bool` documentation pass.
- [ ] 9.3 Complete the `silk/char` documentation pass.
- [ ] 9.4 Complete the `silk/i8` documentation pass.
- [ ] 9.5 Complete the `silk/i16` documentation pass.
- [ ] 9.6 Complete the `silk/i32` documentation pass.
- [ ] 9.7 Complete the `silk/i64` documentation pass.
- [ ] 9.8 Complete the `silk/isize` documentation pass.
- [ ] 9.9 Complete the `silk/u8` documentation pass.
- [ ] 9.10 Complete the `silk/u16` documentation pass.
- [ ] 9.11 Complete the `silk/u32` documentation pass.
- [ ] 9.12 Complete the `silk/u64` documentation pass.
- [ ] 9.13 Complete the `silk/usize` documentation pass.
- [ ] 9.14 Complete the `silk/f32` documentation pass.
- [ ] 9.15 Complete the `silk/f64` documentation pass.
- [ ] 9.16 Audit repeated numeric-family summaries, sections, links, and example selection for semantic consistency without erasing type-specific boundaries.

## 10. Integration and Final Verification

- [ ] 10.1 Run the public-surface inventory again and confirm required module, root declaration, public field, and service or interface operation coverage is complete with no undocumented accidental omissions.
- [ ] 10.2 Audit every existing and new stdlib example, deliberately keeping, fixing, replacing, or removing it; confirm every remaining ignored example is justified and every executable example compiles.
- [ ] 10.3 Audit cross-module semantic links, generated anchors, module summaries, public declaration counts, source order, and documentation-site navigation across all 41 modules.
- [ ] 10.4 Wire the now-green stdlib policy command, doctest command, and generated-output freshness check into the normal compiler package verification reached by root `pnpm check`.
- [ ] 10.5 Regenerate the full documentation tree and verify a second fresh generation produces no diff.
- [ ] 10.6 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test`, fixing documentation-tooling or generated-output failures without weakening the policy.
- [ ] 10.7 Run `pnpm check`, build the documentation application, and inspect representative module pages for hierarchy, Markdown rendering, code highlighting, links, and navigation.
- [ ] 10.8 Run `pnpm release:candidate` because shipped package documentation contents change, and report any failure with whether it predates this change.
- [ ] 10.9 Confirm the final diff contains no unintended runtime, compiler-semantic, public-type, or visibility change and report all separately tracked public-surface concerns.
