## 1. Syntax and formatting

- [x] 1.1 Add `flow`, `run`, and `fail` keywords plus lossless syntax nodes for flow declarations, failure rows, run expressions, and fail statements.
- [x] 1.2 Parse `flow fn`, normalized row syntax, prefix `run`, `fail move`, and explicit catch type arguments on pipeline targets with bounded recovery that preserves following declarations and statements.
- [x] 1.3 Extend syntax traversal and canonical formatting without changing flow-free formatting output.
- [x] 1.4 Add lexer, parser, recovery, and formatter tests for complete and damaged flow/failure forms.

## 2. Header contracts and semantic types

- [x] 2.1 Extend declaration headers with ordinary/flow kind, source-retained failure members, and canonical normalized nominal rows.
- [x] 2.2 Diagnose unknown, inaccessible, generic-open, non-nominal, or damaged failure members deterministically while preserving unavailable facts.
- [x] 2.3 Add the compiler-private statically shaped flow type/contract identity with deterministic equality, ordering, substitution, traversal, and encoding.
- [x] 2.4 Add declaration and type tests for empty, reordered, repeated, imported, generic, invalid, and unavailable rows.

## 3. Semantic flow recipes and failures

- [x] 3.1 Elaborate calls to `flow fn` as lazy direct recipes whose arguments are captured left-to-right and whose body is not entered.
- [x] 3.2 Elaborate immutable local recipe bindings and prefix `run`, rejecting mutable, escaping, dynamically selected, or non-flow operands.
- [x] 3.3 Elaborate `fail move` as an owned nominal terminating transfer and reject failure in ordinary functions or outside the declared row.
- [x] 3.4 Elaborate direct and pipelined `Flow.catch<E>` over a protected recipe and statically known handler flow function, computing the exact residual row.
- [x] 3.5 Check every run's residual row against its containing flow declaration and require an empty row in ordinary functions and executable entry.
- [x] 3.6 Add semantic tests for laziness, success typing, row subtraction/union, invalid handlers, undeclared propagation, non-nominal failure, and explicit stopped facts.

## 4. Ownership and HIR

- [x] 4.1 Track Copy, moved, and borrowed recipe captures; reject recipe escape, mutable replacement, conflicting loans, and repeated run after an affine capture is taken.
- [x] 4.2 Transfer one owned failure payload through propagation or into a matching handler and plan cleanup exactly once for every exited region.
- [x] 4.3 Add HIR flow construction, catch, run, and fail nodes with canonical contracts, targets, capture access, and provenance.
- [x] 4.4 Extend HIR traversal, verification, encoding, unavailable-state handling, and ownership/cleanup projections for the new nodes.
- [x] 4.5 Add focused HIR and ownership tests for delayed captures, repeated runs, propagation cleanup, handler ownership, traps, and malformed facts.

## 5. Instances and target layout

- [x] 5.1 Discover direct flow bodies and handler instances structurally with existing concrete type substitutions and no runtime-outcome specialization.
- [x] 5.2 Add lazy compiler-owned outcome layout entries with success tag zero, canonical nominal failure tags, and shared payload-slot mappings.
- [x] 5.3 Plan target-specific outcome calling lanes for scalar, aggregate, array, union, zero-lane, and `Usize` success/failure shapes without backend vocabulary.
- [x] 5.4 Add instance/layout tests for generic reuse, handler reachability, canonical tags, mixed payload shapes, all targets, and flow-free byte stability.

## 6. Structured MIR and verification

- [x] 6.1 Extend flow MIR functions with normalized failure rows and selected outcome shapes while preserving ordinary function representation.
- [x] 6.2 Lower successful return and `fail` to distinct flow terminal outcomes with exact payload identities and cleanup ordering.
- [x] 6.3 Lower `run` and catch recipes to structured DAG dispatch with success, matching-handler, unmatched-propagation, and trap paths.
- [x] 6.4 Extend MIR traversal, encoding, and verification for instance targets, tags, payload mappings, rows, local types, catch algebra, and cleanup-before-propagation.
- [x] 6.5 Add MIR tests for success, recovery, unmatched propagation, nested call chains, zero-lane values, cleanup, and forged malformed outcomes.

## 7. Evaluation and backend realization

- [x] 7.1 Extend logical evaluation with explicit success/failure outcomes and deterministic ordered call, binding, success, failure, and cleanup events.
- [x] 7.2 Realize compiler-planned flow outcome signatures, returns, calls, tag dispatch, payload mapping, and recovery in native LLVM without unwinding.
- [x] 7.3 Realize the same compiler-planned outcome contract through direct Wasm multi-value results and structured branches.
- [x] 7.4 Preserve trap behavior outside the failure channel and keep flow-free IR, bitcode, WAT, Wasm bytes, and symbols unchanged.
- [x] 7.5 Add evaluator, LLVM, and Wasm tests that execute both canonical success and handled-failure paths plus propagation and trap separation.

## 8. Acceptance, inspection, and determinism

- [x] 8.1 Add a canonical generic fixture covering delayed execution, exact catch, unmatched internal propagation, cleanup, and empty entry residual row.
- [x] 8.2 Add three-engine parity and actual native/Wasm execution checks for success, recovery, and trap paths.
- [x] 8.3 Add fresh-process determinism gates for facts, ownership, HIR, instances, layout, MIR, traces, native artifacts, and Wasm artifacts.
- [x] 8.4 Add pipelined-recovery and stopped-residual presets to the unified `/labs` inspector and expose function kind, failure rows, recipe/run facts, outcome layout, MIR, and execution events.

## 9. Roadmap and verification

- [x] 9.1 Mark `add-usize-scalar` complete and `add-flow-functions-and-typed-failures` implemented in the project roadmap and scoped-allocation prerequisite references.
- [x] 9.2 Run focused compiler and Labs tests, including existing flow-free golden and determinism suites.
- [x] 9.3 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`; report exact provenance for any failure.
- [x] 9.4 Run strict OpenSpec validation and inspect the final diff for requirement/provision, scopes, allocation, suspension, dynamic closure ABI, private inference, or unrelated creep.
