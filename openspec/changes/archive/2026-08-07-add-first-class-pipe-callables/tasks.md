## 1. Callable Syntax and Formatting

- [x] 1.1 Add lossless tokens and syntax nodes for `once fn`, `mut fn`, and ordered callable parameter/result types, with parser recovery tests for every missing boundary.
- [x] 1.2 Refactor call parsing into repeated postfix application over callable-producing expressions and cover named, qualified, bound, grouped, and prior-call callees.
- [x] 1.3 Replace the qualified-only `PipelineTarget` grammar with a complete callable right operand while preserving left associativity and bounded recovery.
- [x] 1.4 Move `run` to complete-expression parsing with comma, delimiter, block, and statement boundaries, including nested and grouped run tests.
- [x] 1.5 Update canonical formatting for callable types, chained applications, multiline callable pipelines, and grouped versus ungrouped `run`, with idempotence coverage.
- [x] 1.6 Update CodeMirror, TextMate, and VS Code syntax artifacts and tests for callable modes without introducing a `dual` keyword.

## 2. Canonical Callable Types and Resolution

- [x] 2.1 Add canonical callable parameter/result types and shared, exclusive, and consuming invocation modes to type equality, ordering, encoding, substitution, and diagnostics.
- [x] 2.2 Represent named builtins, local declarations, imported declarations, and callable bindings as function values without invoking them.
- [x] 2.3 Resolve an `N - 1` argument call to a leading-argument section only for `N >= 2`, retain complete calls at arity `N`, and reject every other under-application.
- [x] 2.4 Infer section capture types and invocation mode from the resolved trailing parameter contracts, including Copy, shared, exclusive, and moved arguments.
- [x] 2.5 Implement callable-mode compatibility so shared reusable values satisfy exclusive or once contracts and exclusive reusable values satisfy once contracts, while rejecting reverse substitutions.
- [x] 2.6 Add focused diagnostics for non-callable application, incompatible callable signatures, invalid invocation access, redundant unary empty calls, and deeper under-application.

## 3. Generics and Semantic Facts

- [x] 3.1 Extend generic inference so section construction records trailing evidence and complete unary application resolves only parameters evidenced by the omitted leading argument.
- [x] 3.2 Keep return-only inference, partial explicit generic lists, and polymorphic recursion rejected for callable sections with stable diagnostics.
- [x] 3.3 Add deterministic semantic facts for function items, section identities, omitted parameter zero, ordered captures, callable modes, and retained dependencies.
- [x] 3.4 Replace inserted-pipeline-argument facts with left input, callable right expression, unary application contract, result, and left-first evaluation provenance.
- [x] 3.5 Make run facts reference the complete Effect-producing operand selected by the new grammar and preserve exact residual rows and one-layer result typing.
- [x] 3.6 Add semantic and fresh-process encoding tests for direct calls, sections, stored callables, generic application, inaccessible targets, and unavailable callable dependencies.

## 4. HIR and Ownership

- [x] 4.1 Add canonical HIR function-item, callable-section, environment-capture, and callable-application expressions with hidden construction-site identities.
- [x] 4.2 Lower pipelines into ordinary unary callable application and erase non-escaping sections into direct calls only when identity, order, ownership, and provenance remain equivalent.
- [x] 4.3 Preserve stored and cross-call callable environments in HIR and reject unknown-sized owned erased returns or heterogeneous callable joins during bootstrap.
- [x] 4.4 Extend ownership to track callable environment slots, Copy snapshots, shared and exclusive loans, moved owners, retained dependencies, and deterministic drop.
- [x] 4.5 Derive shared, exclusive, or take-once invocation from environment use and reject repeated consuming calls, overlapping exclusive access, and provider movement while retained.
- [x] 4.6 Verify that pipeline application evaluates and transfers its affine left value once before constructing or accessing the right callable.
- [x] 4.7 Add HIR and ownership encodings and tests for uncalled drop, successful capture transfer, typed Effect failure, second-call rejection, and the existing trap cleanup boundary.

## 5. Instances, Layout, and MIR

- [x] 5.1 Add hidden callable construction identities and concrete capture substitutions to deterministic runtime instance discovery and recursion termination.
- [x] 5.2 Plan target-aware environment layout and call-scoped code/environment views without making any allocator implementation or universal heap box compiler-special.
- [x] 5.3 Add backend-neutral MIR operations for callable construction, ordered captures, shared, exclusive, and consuming application, and environment cleanup in the structured DAG.
- [x] 5.4 Lower complete run operands so Effect combinators execute before ungrouped run and grouped run results remain ordinary callable inputs.
- [x] 5.5 Extend MIR verification for callable signatures, concrete identities, mode access, capture transfers, dependency order, cleanup, and absence of open generic environments.
- [x] 5.6 Add deterministic MIR text/encoding tests for erased immediate sections, stored environments, generic callbacks, consuming calls, and grouped run.

## 6. Evaluator and Effect Composition

- [x] 6.1 Add evaluator callable values with deterministic hidden identity, ordered environment slots, invocation state, and cleanup independent of JavaScript closure identity or garbage collection.
- [x] 6.2 Execute shared and exclusive callables repeatedly, consume take-once environments exactly once, and expose stable blocked outcomes for invalid reuse.
- [x] 6.3 Implement ordinary callable contracts for `Effect.map`, `flatMap`, `tap`, `catch`, and the other bootstrap higher-order operations needed by the accepted Effect examples.
- [x] 6.4 Derive composed Effect run access from both the input Effect and stored callback, and make retry reject any take-once component before execution.
- [x] 6.5 Preserve map nesting, flatMap flattening, tap result preservation, and effectful Logger requirements without adding a non-effect tracing intrinsic.
- [x] 6.6 Add evaluator traces and tests for arithmetic mapping, effectful logging through tap, mutable callbacks across runs, owned callback rejection, retry, and run grouping.

## 7. LLVM and WebAssembly Realization

- [x] 7.1 Extend LLVM lowering for compiler-planned callable environments, direct-erased sections, call-scoped views, shared/exclusive access, consuming transfer, and cleanup.
- [x] 7.2 Extend direct WebAssembly lowering for the same logical callable MIR while choosing only target-local environment and indirect-call details.
- [x] 7.3 Verify both backends preserve left-first pipeline evaluation, one invocation, capture lifetime, target-aware layout, and grouped run behavior.
- [x] 7.4 Add native/Wasm/evaluator parity tests for reusable, exclusive, take-once, generic, Effect-composed, and dropped-uninvoked callables.
- [x] 7.5 Add fresh-process determinism tests for callable identities, layouts, MIR, symbols, LLVM IR/bitcode, and Wasm bytes.

## 8. Unified Labs and Language Definitions

- [x] 8.1 Add unified `/labs` presets for named function values, automatic sections, stored callbacks, all three invocation modes, and invalid consuming reuse.
- [x] 8.2 Add Effect presets for `map(I32.add(2))`, effectful logging through tap or flatMap, mutable callback state, retry rejection, and nested map results.
- [x] 8.3 Add paired grouped and ungrouped `run` presets and make every inspector pane explain composition-before-run versus transformation-after-run.
- [x] 8.4 Render callable semantic facts, capture environments, ownership state, HIR, instances, layout, MIR, evaluator events, and backend realization with accessible text equivalents.
- [x] 8.5 Update Wayfinder ownership, type, function-contract, compiler-pipeline, runtime, and syntax issues so first-class callables replace the old pipeline-insertion claim consistently.
- [x] 8.6 Update the prototype syntax examples and Effect pattern corpus with reusable, exclusive, consuming, generic, logging, and allocation-bearing callable scenarios.

## 9. Migration and Verification

- [x] 9.1 Remove the legacy `PipelineTarget` semantic path, inserted-argument facts, pipeline-specific Effect combinator branches, and obsolete Flow terminology after all callers migrate.
- [x] 9.2 Migrate compiler fixtures, formatter snapshots, goldens, documentation examples, and Labs presets to callable application and low-precedence `run`.
- [x] 9.3 Run strict OpenSpec validation and the focused parser, formatter, semantic, HIR, ownership, MIR, evaluator, backend, language-highlighting, and Labs suites.
- [x] 9.4 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`, reporting any pre-existing warnings separately.
- [x] 9.5 Run `pnpm release:candidate` because compiler, language, VS Code, and docs package contents or exports may change.
