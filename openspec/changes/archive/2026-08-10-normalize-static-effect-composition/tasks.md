## 1. Characterize the First Slice

- [x] 1.1 Extend the cost harness with entry-local direct-Wasm calls and stable per-case applicability expectations.
- [x] 1.2 Define deterministic accepted and first-rejection verdicts for constructor shape, direct target, local single use, capture ownership, synchronous vocabulary, size, and depth.
- [x] 1.3 Encode verdicts in MIR and expose them through the analysis facade.
- [x] 1.4 Add verifier coverage for dangling verdict provenance/identities and inconsistent direct-run capture facts.

## 2. Implement Shared Normalization

- [x] 2.1 Add one backend-neutral normalization actor after lowering and before evaluation or backend emission.
- [x] 2.2 Fold direct calls/applications whose complete concrete body is one `MakeEffect` and return, with total deterministic parameter substitution.
- [x] 2.3 Replace a local single-use Copy/shared `MakeEffect` plus `RunEffectValue` with `RunStaticEffect`, retaining providers, failure mappings, releases, and provenance.
- [x] 2.4 Implement `RunStaticEffect` verification, encoding, evaluation, LLVM, and direct-Wasm behavior with `RunEffectValue` runner-call and semantic-observation parity.
- [x] 2.5 Reject complex, escaping, reused, recursive, affine/exclusive, cross-region, oversized, or suspension-unknown candidates without changing their MIR.
- [x] 2.6 Make normalization idempotent and retain ordinary MIR byte-for-byte for rejected candidates.

## 3. Prove Semantic and Structural Parity

- [x] 3.1 Compare eligible cost cases through normalized and explicitly unnormalized MIR for values, failures, evaluation order, requirements, traps, allocations, and cleanup observations.
- [x] 3.2 Add a copied user-defined constructor and prove identical normalization without Effect namespace privilege.
- [x] 3.3 Add ineligible controls for complex constructors, escaping/reused values, affine captures, and synthetic unknown suspension.
- [x] 3.4 Assert eligible direct-Wasm entries lose foldable constructor calls while native entry structure does not regress.
- [x] 3.5 Assert an explicit affine-capture control remains unnormalized while the allocation corpus preserves exactly one Payload Drop and balanced release.
- [x] 3.6 Normalize twice and in fresh processes to prove idempotent MIR, deterministic verdicts, and byte-identical backend artifacts.

## 4. Integrate and Document

- [x] 4.1 Enable normalization by default before MIR evaluation and both backend emissions, with no backend-local duplicate pass.
- [x] 4.2 Update MIR/pipeline documentation, the cost report, roadmaps, and inspector descriptions with the shipped boundary and remaining runner call.
- [x] 4.3 Write a separate OpenSpec proposal for guarded cross-function runner CFG inlining if entry-local evidence still justifies it.
- [x] 4.4 Run focused MIR, evaluator, native LLVM, direct-Wasm, tooling, and fresh-process cost tests.
- [x] 4.5 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and strict OpenSpec validation.
