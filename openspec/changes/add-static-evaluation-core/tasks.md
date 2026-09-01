## 1. Add the static syntax and declaration surface

- [x] 1.1 Add the `static` and `compileError` keywords and lossless syntax nodes for `static fn`,
  static parameters, `let static`, `static if`, and the dedicated `compileError(message)`
  expression, including bounded recovery and rejection of conditional declarations; verify lexer,
  parser, round-trip, recovery, and non-shadowability tests cover every form.
- [x] 1.2 Extend declaration signatures, module surfaces, and inspection encodings with static
  function and parameter facts while retaining source-provenanced body templates for functions
  that require specialization; verify module-surface round trips and tooling snapshots remain
  deterministic.
- [x] 1.3 Add dedicated diagnostic catalog entries for phase violations, selected compile errors,
  evaluation cycles, and each evaluation budget; regenerate the catalog and verify tests assert
  diagnostic codes and exact source spans rather than wording.

## 2. Build the deterministic static evaluator

- [x] 2.1 Add focused `StaticValue` and `StaticEvaluation` actor modules with an immutable canonical
  value algebra, stable encoding, target environment, and admissibility checks that remain separate
  from runtime `Copy`; verify equal values encode identically and rejected resource-bearing values
  cannot enter evaluator state.
- [x] 2.2 Evaluate literals, primitive and enum operations, static text inspection, pure aggregate
  construction, static function calls, and complete return values; verify nested static calls and
  target-profile evaluation with semantic tests at the `Analysis.evaluate` tier.
- [x] 2.3 Evaluate static locals, ordinary control flow inside `static fn`, and `let mut` through
  complete binding replacement while rejecting borrows, in-place mutation, ordinary functions,
  runtime bindings, Effects, services, unsafe operations, and external access; verify both accepted
  accumulator loops and each rejected phase crossing.
- [x] 2.4 Implement `compileError` as `StaticFailure.CompileError` for the selected specialization
  and add deterministic cycle, step, call-depth, retained-value, and residual-growth enforcement
  with source-level static traces; verify boundary cases produce no partial value or residual body
  and repeat with identical facts.

## 3. Residualize mixed functions before the runtime pipeline

- [x] 3.1 Elaborate the condition and only the selected arm of `static if` from the retained body
  template, while preserving parser diagnostics from both arms; verify inactive unknown names,
  types, effects, requirements, and calls create no semantic facts.
- [x] 3.2 Residualize mixed functions so static parameters and locals control specialization while
  selected ordinary operations and runtime values become ordinary typed HIR; verify `compileError`
  acts as bottom and return analysis considers only the selected residual control flow.
- [x] 3.3 Select the concrete target before constructing the deterministic executable worklist, then
  extend instance identity with canonical static arguments and the containing target realization;
  record each key before following residual calls and verify equal applications deduplicate, unequal
  values remain distinct, and recursive specializations terminate.
- [x] 3.4 Close a private residual application graph from direct calls plus a target-neutral
  cleanup-edge prepass, then run ownership and cleanup exactly once per successful residual
  specialization before admitting executable reachability, omitting static bindings, evaluator
  storage, and inactive-arm operations; verify the prepass publishes no ownership facts, liveness
  and cleanup goldens contain only residual runtime values, and no declaration-level ownership fact
  is reused.
- [x] 3.5 Publish residual HIR, call edges, static provenance, and specialization identities through
  `Analysis.realize` and existing inspectors without exposing evaluator identities; verify repeated
  realizations produce byte-identical semantic encodings.

## 4. Replace target selectors and generalize typed constants

- [x] 4.1 Add a `StaticOnly` intrinsic classification and the single sealed
  `Intrinsic.targetProfile() -> u8` primitive, implemented only by static evaluation; verify the
  catalog assigns it no evaluator, LLVM, or WebAssembly runtime target and rejects runtime calls.
- [x] 4.2 Implement the ordinary `silk.target` source actor whose zero-argument static functions map
  the four frozen profile codes to nominal target and architecture enums and whose primitive
  constants derive pointer-width and integer-limit facts; verify its public API for every canonical
  target profile without expanding the primitive-only constant contract.
- [x] 4.3 Retain explicitly typed primitive constant declarations as target-neutral type, initializer
  template, and provenance facts, then evaluate them through the shared static evaluator in each
  concrete target realization, retaining no runtime storage or initializer while continuing to
  reject inference and aggregate constants; verify literal, computed, mismatched, cyclic,
  target-dependent, and cross-module initializers.
- [x] 4.4 Migrate `usize` and `isize` constants to the ordinary target actor, delete `TargetConstant`
  and every compiler-known `Target.<fact>` selector path, and update affected fixtures; verify no
  obsolete selector symbol or dual path remains in source, tests, generated files, or docs.
- [x] 4.5 Run intrinsic availability only after residual call-closure discovery; verify a restricted
  intrinsic in a selected arm is rejected for the target, while inactive and unreachable calls are
  absent from availability facts and backend inventories.

## 5. Complete acceptance coverage and reference documentation

- [x] 5.1 Add end-to-end semantic fixtures for target selection, mixed static/runtime formatting-like
  specialization, static argument reuse, and unreachable static failures, using one shared analysis
  snapshot per source and adding backend legs only where code generation itself is under test.
- [x] 5.2 Add committed deterministic encodings for static traces, residual HIR, specialization keys,
  ownership, and cleanup, relying on the existing global fresh-process determinism canary rather
  than introducing per-feature fresh-process tests.
- [x] 5.3 Update the prescriptive language reference for static functions, parameters, bindings,
  conditionals, compile errors, target enums, value semantics, mutation limits, diagnostics, and the
  boundary between static and runtime work; verify documentation examples parse under the
  implemented grammar.
- [x] 5.4 Regenerate affected standard-library and compiler artifacts, then run `pnpm typecheck`,
  `pnpm exec biome check .`, `pnpm test`, and `pnpm check` in that order; because package contents
  change, also run `pnpm release:candidate` and report any failure with its exact command and whether
  it predates this change.
