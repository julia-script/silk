## 1. Applied Qualifier Syntax

- [x] 1.1 Add a neutral lossless syntax representation for `Path<Arguments>.member`, separate owner arguments from call-owned type arguments, and verify direct/pipeline interface forms plus existing applied nominal-union values, constructors, patterns, and formatting round-trip without regression.
- [x] 1.2 Add local recovery for missing qualifier delimiters and operation names, and verify damaged applied qualifiers preserve the following statement with deterministic syntax diagnostics.
- [x] 1.3 Expose applied qualifier arguments and spans through syntax accessors and semantic occurrence views, and verify tooling snapshots distinguish `Interface<A>.operation` from `Actor.operation<A>`.

## 2. Interface Application and Provider Resolution

- [x] 2.1 Resolve and normalize the complete written interface application before operation lookup, and verify visibility, unknown-member, arity, and kind failures use source diagnostics without attempting provider selection.
- [x] 2.2 Infer implicit `Self` first from agreeing supplied operation operands and only then from one matching fallback bound, construct the static applied-operation contract, and verify a provider with `Encodable<u32>` and `Encodable<string>` selects the explicitly written application in both cases.
- [x] 2.3 Reuse ordinary call-contract checking and conformance proof for the selected application, and verify owned, shared, exclusive, ordinary, effectful, failure, and requirement contracts retain their declared shapes without adaptation.
- [x] 2.4 Reject undetermined, conflicting, missing, and ambiguous provider evidence without consulting expected results, reject applied service operations, and verify zero-operand, inconsistent-`Self`, multiple-bound, missing-conformance, service-owner, and result-annotation cases produce deterministic diagnostics.

## 3. Callable Sections and Pipelines

- [x] 3.1 Carry an unresolved provider only inside static applied-operation section inference, complete it from the pipeline's left operand, and verify `&age |> Encodable<u32>.encode` selects the same application, provider, and witness as the equivalent direct call.
- [x] 3.2 Reject an applied operation section that escapes without a resolved provider, and verify storing `Encodable<u32>.encode` creates no executable polymorphic callable or witness dictionary.
- [x] 3.3 Preserve greedy `run` and one-layer Effect execution over applied-operation pipelines, and verify direct and piped effect calls produce the same evaluated `u32` and `string` results.

## 4. HIR, Witnesses, and Realization

- [x] 4.1 Generalize bound-only resolution/HIR concepts into canonical static interface-operation evidence with normalized application arguments, provider, substituted contract, operation, and provenance, and verify direct and piped HIR snapshots differ only where source provenance requires it.
- [x] 4.2 Route applied calls through existing executable witness discovery and static witness lowering, and verify each concrete application reaches its own inline or mapped implementation in evaluator and Wasm execution.
- [x] 4.3 Publish witness Effect sites, constructors, and runners only for fully available applied calls, and add a regression for the reported `Effect entry lowering lost its constructor or runner` source proving invalid forms stop with diagnostics and valid forms realize successfully.
- [x] 4.4 Add the valid multi-application effect program to the shared native differential corpus, and verify the designated acceptance suite agrees with evaluator behavior without adding a per-feature native compilation test.

## 5. Language Reference and Verification

- [x] 5.1 Update the prescriptive interface and expression reference with applied qualified calls, direct/pipeline equivalence, operand-first provider inference, and explicit exclusions for method syntax, services, and result-directed selection; verify documentation links and Silk examples are valid.
- [x] 5.2 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test`, fixing change-caused failures and recording any unrelated pre-existing failure.
- [x] 5.3 Run `pnpm check` and verify the complete repository gate passes before handoff.
