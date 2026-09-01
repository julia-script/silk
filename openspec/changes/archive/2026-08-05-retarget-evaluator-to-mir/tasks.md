## 1. Corpus scaffold

- [x] 1.1 Create `packages/compiler/test/support/corpus.ts`: shared programs (literal, identity,
      two-parameter, nested, siblings, recursion, trap cases) with pinned expected outcomes — the
      differential-harness scaffold the native acceptance will reuse

## 2. MIR interpreter

- [x] 2.1 Rewrite `packages/compiler/src/BootstrapEvaluation.ts` as the CFG interpreter:
      `evaluate(discovery, program)` executing locals, blocks, operations, and terminators from the
      resolved entry
- [x] 2.2 Trace events re-tied to MIR (entry, call, binding with ordinals/values/fromCall,
      return) with canonical identities and lowered provenance
- [x] 2.3 Blocked outcomes on MIR vocabulary: discovery entry reasons, executed traps, missing
      lowered functions, recursive cycles with ordered cycles and closing spans
- [x] 2.4 Rewrite evaluation tests against the corpus: exact results, trace shapes, partial
      blocked prefixes, recursion, determinism

## 3. Facade and inspector

- [x] 3.1 `Analysis.evaluate(snapshot)` passes discovery and the lowered program; facade tests
      updated
- [x] 3.2 Rewire the evaluation panel (trace labels, blocked summaries) onto the new events and
      reasons
- [x] 3.3 Rewire the flow model's Evaluated overlay: call/binding/return matching by span,
      ordinals, and canonical identity; parameter-read evidence retires
- [x] 3.4 Update inspector and flow-model tests to the MIR trace and trap vocabulary

## 4. Verification

- [x] 4.1 Full compiler and docs suites pass; `pnpm check` and release-candidate green
- [x] 4.2 `openspec validate retarget-evaluator-to-mir --type change --strict` passes
