## 1. Contract and evidence before compiler edits

- [x] 1.1 Complete the prescriptive reference for parameter syntax, manifest binding transport, profile domains and intrinsic signatures; reconcile the target-layout/project specs with the deltas and verify OpenSpec strict validation. This must precede compiler implementation.
- [x] 1.2 Pin normative platform authority revisions and exact Clang/object tool versions for the four primitive-fact fixtures; record source digests, admitted facts, missing analogues and deliberate divergences in a provenance artifact. Verify no host-header fallback or claim of aggregate ABI proof.
- [x] 1.3 Inventory every ordinal target API, cache identity, compilation request, tooling selector and source consumer; verify the inventory covers generated catalogs, examples and tests and identifies replacement ownership.

## 2. Logical profiles and target descriptions

- [x] 2.1 Implement separate typed logical domains and versioned descriptions for all four targets; verify missing/inconsistent facts and unsupported combinations fail before use with structured diagnostics.
- [x] 2.2 Implement canonical immutable profile construction and tagged value encoding; verify independently allocated/reordered equal inputs encode equally while changed logical choices differ and physical paths/provenance cannot affect semantic identity.
- [x] 2.3 Add pinned LLVM and independent header-free C/object fixtures for the admitted primitive facts; verify all four target descriptions against those fixtures with exact tool provenance.

## 3. Source schema and binding surface

- [x] 3.1 Implement unconditional param declaration parsing and source-preserving formatting, including optional defaults and where predicates; verify parser cases and syntax diagnostics using shared syntax snapshots.
- [x] 3.2 Implement declaration/type facts, stable package/module/parameter identity and public/private visibility; verify imported aliases preserve identity, unknown/private bindings fail and unequal sources cannot claim one package identity.
- [x] 3.3 Implement admitted integer, boolean, string, enum, optional, array and record binding conversion with precise type/range checks; verify nested values and wrong nominal/field/element types at the analysis tier.
- [x] 3.4 Implement origin-bearing binding tiers and provenance rejection; verify same-tier conflicts, missing required values, deterministic public translation and secret/runtime/physical-supply rejection without value disclosure.

## 4. Bootstrap and ordinary static evaluation

- [x] 4.1 Separate immutable initial facts and private bootstrap state from completed profiles; discover unconditional schemas before ordinary specialization and verify failed bootstrap publishes no profile.
- [x] 4.2 Resolve defaults through the existing static evaluator with imported/forward helpers and final-value dependencies; verify target-dependent defaults, overridden cycles, genuine value/schema cycles and uncalled helpers without a second evaluator.
- [x] 4.3 Run all source predicates against final values before publication; verify false predicates, compileError traces, validation of explicit overrides and stable diagnostic origins/order.
- [x] 4.4 Key bootstrap results by initial inputs and demanded source dependencies, and completed evaluation by canonical profile identity; verify changed helper/default invalidation and same-target false/true profiles coexisting in one process.

## 5. Intrinsics, compilation and tooling

- [x] 5.1 Replace targetProfile and the source Profile enum with minimal individual static fact intrinsics and ordinary Silk wrappers; verify runtime rejection, narrow architecture/OS/width selection and regenerated catalog consistency.
- [x] 5.2 Propagate completed profile identity through analysis, semantic/compilation requests, backend emission identity and diagnostics; verify no independent target/artifact/mode cache path can omit configuration.
- [x] 5.3 Implement project named/default profiles, typed binding transport, complete override and target shorthand at CLI/LSP application edges; verify conflicting modes, cross-compilation and explicit host fallback, with identical compiler/tooling identities.
- [x] 5.4 Update all inventoried source consumers, docs, examples, snapshots and exports and delete superseded ordinal paths; verify an absence audit and public export inventory with no compatibility shim.

## 6. Integration verification

- [ ] 6.1 Run pnpm typecheck, pnpm format:check, pnpm lint, pnpm test, then pnpm check; record exact failures and whether they predate this change. Verify all required commands pass before marking implementation complete.
- [ ] 6.2 Run pnpm release:candidate because compiler/stdlib package contents change; verify the package/export checks and generated artifacts pass and review the final diff against every JUL-120 acceptance item.
