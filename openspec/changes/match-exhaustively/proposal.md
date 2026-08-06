## Why

Silk can now construct, transport, widen, mutate, and clean structural unions, but it cannot inspect
their active nominal member or recover a precise payload. Exhaustive mode-aware matching is the
missing operation that turns the completed data and control foundations into useful branching over
compiler-shaped values.

## What Changes

- Add lossless match expressions over nominal values and normalized structural unions, with nominal
  struct patterns, nested field bindings, explicit `..`, guarded arms, and a universal `_` arm.
- Make match access explicit: Copy-only bare matching, whole-value consuming `match move`, and
  match-local shared or exclusive lexical borrows through `match &` and `match &mut`.
- Check coverage by canonical union set subtraction. Unguarded member arms remove members, guards do
  not prove coverage, and duplicate, unreachable, guard-after-exhaustive, or incomplete matches are
  deterministic source diagnostics.
- Infer a match result from reachable arms: identical results remain precise, nominal and union
  results normalize together, `Never` contributes no member, and incompatible scalar or aggregate
  mixtures are rejected.
- Preserve affine ownership during destructuring: consuming patterns transfer bound fields and clean
  omitted fields exactly once; borrowed patterns cannot move or escape their arm; the original owner
  remains available after borrowed matching.
- Represent matching as explicit acyclic HIR and MIR regions over canonical member identities.
  Numeric tags and backend control remain absent from compiler-owned relationships; native LLVM and
  direct WebAssembly privately realize dispatch from the verified layout plan.
- Extend evaluation, differential and determinism gates, facade queries, language highlighting, and
  the existing unified `/labs` panes and presets through the complete matching slice.
- **BREAKING**: reserve `match` as a language keyword and recognize the match punctuation needed for
  access modes, arms, guards, patterns, and universal coverage.

## Capabilities

### New Capabilities

- `bootstrap-exhaustive-matching`: Match access modes, nominal patterns, guards, canonical
  exhaustiveness, narrowing, pattern bindings, and result-type joins.

### Modified Capabilities

- `bootstrap-lexer`: Recognize match keywords and punctuation losslessly without disturbing existing
  operators.
- `bootstrap-syntax`: Parse and recover mode-aware match expressions, arms, guards, and nominal
  patterns in every expression position.
- `silk-source-formatting`: Format match expressions, patterns, guards, and arms idempotently while
  preserving damaged syntax.
- `bootstrap-semantic-facts`: Publish pattern lookup, binding, coverage, narrowing, guard, and result
  facts with deterministic diagnostics.
- `bootstrap-hir`: Represent match dispatch, narrowed member payloads, guarded arms, and joined
  results as an acyclic region structure.
- `bootstrap-ownership`: Check Copy, consuming, shared, and exclusive match modes and exact
  branch-local cleanup.
- `bootstrap-instances`: Follow nominal members, pattern-bound payloads, branch results, and cleanup
  reachable through matches.
- `bootstrap-mir`: Carry verified member dispatch, arm regions, bindings, guards, joins, and cleanup
  without backend-owned tags or cyclic control.
- `bootstrap-evaluation`: Execute match dispatch and pattern binding from logical active-member values
  with deterministic traces.
- `bootstrap-backend`: Realize verified matches privately in native LLVM and direct WebAssembly while
  preserving compiler-owned layouts and DAG relationships.
- `bootstrap-compiler-driver`: Add valid, invalid, trapping, ownership, and fresh-process matching
  programs to the three-engine differential corpus.
- `bootstrap-analysis-facade`: Expose immutable match facts and cross-phase provenance without tooling
  reconstructing coverage or payload selection.
- `bootstrap-syntax-inspector`: Inspect matching through the existing unified `/labs` panes and
  coordinated selections rather than a standalone inspector.
- `language-codemirror`: Highlight match keywords, modes, patterns, guards, and arms consistently.
- `language-textmate`: Tokenize the accepted match surface consistently in TextMate and generated VS
  Code grammars.

## Impact

- Compiler frontend: token vocabulary, concrete syntax, recovery, semantic facts, diagnostics, name
  resolution for pattern bindings, type joining, and HIR construction.
- Ownership/runtime pipeline: match-local borrow facts, consuming destructuring, cleanup planning,
  instance discovery, MIR verification/lowering, and logical evaluation.
- Backends: private member dispatch and payload projection using the existing compiler-owned union
  layout and calling shapes.
- Tooling: analysis facade, deterministic encoders, formatter, CodeMirror/TextMate/VS Code grammar,
  unified `/labs` rows, presets, and release-candidate coverage.
- No new dependency, stable external ABI, general borrow expression, borrowed function contract,
  array pattern, typed failure handler, or general pattern-alternative system is introduced.
