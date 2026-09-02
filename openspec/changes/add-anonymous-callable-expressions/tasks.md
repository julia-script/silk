## 1. Syntax and formatting

- [x] 1.1 Add a dedicated lossless anonymous-callable syntax node for ordinary and effectful forms, disambiguate `effect fn` from `effect {}`, and verify focused parser acceptance tests pass.
- [x] 1.2 Add bounded recovery for missing parameters, arrows, result/effect contracts, and bodies while rejecting expression-level modifiers, and verify parser damage/recovery tests terminate without token loss.
- [x] 1.3 Format complete ordinary and effectful anonymous callables canonically without repairing damaged syntax, and verify existing formatter corpus and idempotence tests pass.

## 2. Semantic facts and capture analysis

- [x] 2.1 Generalize function-body analysis for a nested anonymous lexical scope with explicit parameters, result, failure, and requirement contracts, and verify body return/effect diagnostics use the written contract.
- [x] 2.2 Generalize free-reference collection to exclude body-owned bindings, deduplicate canonical roots in first lexical occurrence order, and verify parameter/local shadowing plus declaration-order-independent capture ordering.
- [x] 2.3 Classify capture access through existing ownership rules and derive `fn`, `mut fn`, or `once fn`, and verify Copy/shared, exclusive/mutating, moved affine, and incompatible expected-mode cases.
- [x] 2.4 Enforce first-slice exclusions for nested, self-recursive, independently generic, modifier-bearing, and overload-participating forms, and verify each produces one focused semantic diagnostic without executable facts.
- [x] 2.5 Integrate explicit anonymous callable contracts with supplied-argument generic inference while prohibiting expected-result back-inference, and verify enclosing substitutions specialize finitely.

## 3. HIR and executable discovery

- [x] 3.1 Add HIR anonymous-callable construction facts with stable source site, explicit contract/source kind, derived mode, ordered captures, and surrounding substitution, and verify identical occurrences retain distinct deterministic identities.
- [x] 3.2 Elaborate accepted bodies into a hidden executable catalog with authored parameters retaining source ordinals, appended capture parameters, and remapped free references, and verify hidden targets never enter module headers, surfaces, imports, or declaration tooling.
- [x] 3.3 Reuse the named effect-function wrapper for hidden effectful bodies so invocation constructs a lazy Effect, and verify literal construction, invocation, and `run` remain observably distinct stages.
- [x] 3.4 Generalize executable lookup, dependency discovery, instance specialization, and residualization to include hidden targets, and verify a hidden body calling another executable is discovered and specialized.

## 4. Ownership, layout, and MIR

- [x] 4.1 Generalize section-specific callable-environment ownership and layout helpers to all exact environment-bearing callables, and verify source-order acquisition, escape loans, reverse cleanup, and dropped-uninvoked cleanup.
- [x] 4.2 Lower anonymous construction through the existing exact `MakeCallable` representation while preserving separate capture and authored-parameter ordinals, and verify structural HIR/MIR tests cover an order mismatch.
- [x] 4.3 Admit hidden anonymous instances through ordinary MIR function lowering and `ApplyCallable` operand assembly, and verify the MIR verifier rejects target/signature/mode mismatches, duplicated transfer, and repeated `once fn` use.

## 5. Execution and backend parity

- [x] 5.1 Execute ordinary and effectful anonymous targets in the evaluator without JavaScript closure identity and verify an inline `Effect.catchAll` handler completes with `42`.
- [x] 5.2 Add one environment-bearing Wasm case that exercises capture order, invocation, and cleanup, and verify it agrees with the shared semantic/evaluator result.
- [x] 5.3 Add the representative program to the native acceptance corpus and verify native parity without a per-feature native compile or fresh-process determinism test.

## 6. Tooling and language reference

- [x] 6.1 Expose anonymous parameters, locals, captured outer occurrences, explicit contracts, derived modes, and capture summaries through shared semantic/tooling facts, and verify occurrences inside the body resolve to their canonical bindings.
- [x] 6.2 Render anonymous hover without a fabricated name or declaration target, and verify focused ordinary and effectful hover cases.
- [x] 6.3 Complete anonymous lexical scopes and expression starts while excluding declaration-only modifiers, and verify focused completion ordering and recovery cases.
- [x] 6.4 Update the prescriptive callable, effect, ownership, and syntax reference pages with anonymous-callable semantics and exclusions, and verify documentation checks and links pass.

## 7. Review and repository verification

- [x] 7.1 Run focused parser, formatter, analysis/HIR/ownership/MIR, evaluator, Wasm, native-corpus, hover, and completion tests and verify every JUL-72 acceptance path is covered at its cheapest tier.
- [ ] 7.2 Run the mandatory independent test-economics review against the committed diff, resolve every in-scope finding, and record the final reviewer verdict.
- [x] 7.3 Run `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, and `pnpm test` in order, fixing any change-caused failure.
- [x] 7.4 Run `pnpm check` and verify the complete repository gate passes.
- [x] 7.5 Run `pnpm release:candidate` if package contents or exports changed and verify the release-candidate gate passes or record why it is not applicable.
