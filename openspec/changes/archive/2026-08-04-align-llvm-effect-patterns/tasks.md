## 1. Repository Policy and Effect-Native Tests

- [x] 1.1 Update `AGENTS.md` so the current Effect patterns skill governs error semantics, internal/public function authoring, scoped lifecycles, dual APIs, and `@effect/vitest`; remove the superseded `ManagedRuntime` and generic `SilkError` rules.
- [x] 1.2 Add the compatible `@effect/vitest` development dependency to `@silk-lang/llvm` and update the workspace lockfile.
- [x] 1.3 Convert pure byte-string and bitstream cases to ordinary `@effect/vitest` tests with `assert`, without manufacturing Effects solely to run assertions.
- [x] 1.4 Convert all Effect-returning package tests to `it.effect`, use `it.layer` only for genuinely shared service graphs, replace `expect` with `assert`, and remove every test `ManagedRuntime`.
- [x] 1.5 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test`; resolve all hygiene-tier failures before proceeding.

## 2. Function Authoring and Internal Type Safety

- [x] 2.1 Split integer and floating compare descriptions into a discriminated union whose `kind` determines its predicate type, then remove predicate casts from text and bitcode encoding.
- [x] 2.2 Narrow value/constant operands by their handle tags and type intrinsic recipe tables as partial intrinsic-id records, removing the avoidable casts in `FunctionBodyState` and `Intrinsic`.
- [x] 2.3 Convert reusable intrinsic signature recipes, metadata forwarding functions, and other plain Effect-returning helpers to `Effect.fnUntraced`; keep named public actor operations as their existing tracing boundaries.
- [x] 2.4 Scan `packages/llvm/src` for remaining reusable arrow-to-`Effect.gen` functions, non-`const` casts, non-null assertions, and suppressions; repair every instance not justified by a TypeScript expressiveness gap.
- [x] 2.5 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test`; resolve all authoring/type-safety failures before proceeding.

## 3. LLVM-Specific Error Contract

- [x] 3.1 Add the `LlvmError` actor with the `LlvmError` Effect tag, stable `operation`/`message` fields, and discriminated reasons for invalid input, invalid state/ownership, and wrapped causal failures.
- [x] 3.2 Add focused tests for `LlvmError` yieldability, `catchTag('LlvmError')`, semantic validation details, and genuine JavaScript causal ancestry.
- [x] 3.3 Replace `SilkError` imports, signatures, constructors, type annotations, test expectations, and TSDoc examples throughout `packages/llvm` with the new contract.
- [x] 3.4 Replace the source actor, root export, and package subpath with `LlvmError`; remove `SilkError` without a compatibility alias and add public import/export coverage.
- [x] 3.5 Run a repository-wide search to verify no source, test, documentation, package metadata, or release assertion still refers to `SilkError`.
- [x] 3.6 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test`; resolve all error-contract migration failures before proceeding.

## 4. Typed Validation and Synchronous Transitions

- [x] 4.1 Add a coherent internal integer-input actor that validates finite safe integer numbers before `BigInt` conversion and returns operation-specific `LlvmError` failures.
- [x] 4.2 Migrate alignment and attribute integer inputs, including integer lists, to the shared normalization boundary and add fractional, `NaN`, infinity, unsafe-number, negative, and upper-bound tests.
- [x] 4.3 Migrate constant integers, array lengths, target-extension integer parameters, and metadata integer fields to typed normalization and add representative defect-vs-failure regression tests.
- [x] 4.4 Change builder and function-body mutation callbacks to a synchronous `Result` contract that is evaluated under the semaphore and translated into the Effect channel.
- [x] 4.5 Convert ownership and handle resolution (`OwnedHandle`, `Handle`, and `GlobalState`) from thrown yieldable errors to typed results, preserving operation-specific diagnostics.
- [x] 4.6 Migrate module-level actors (`Builder`, `Global`, `Variable`, `Alias`, `Function`, `Type`, `Constant`, `Attribute`, and `Metadata`) to typed transition failures without partial mutation.
- [x] 4.7 Migrate `FunctionBodyState`, `Block`, `Value`, and all `FunctionBody` operations to typed transition failures while preserving synchronous serialized mutation.
- [x] 4.8 Replace yieldable errors thrown by private bitstream/renderer code with private implementation failures and translate them once at `Bitcode.encode` or `IrText.render`.
- [x] 4.9 Make public alignment encoding return an Effect with `LlvmError`, provide a private synchronous encoder path for bitcode emission, and test both valid and oversized alignments.
- [x] 4.10 Search all public and internal LLVM source for `throw new LlvmError`; remove every occurrence and confirm that expected failures use the typed channel.
- [x] 4.11 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test`; resolve all boundary-tier failures before proceeding.

## 5. Scoped Function-Body Lifecycle

- [x] 5.1 Refactor `Function.buildBody` to acquire its reservation and draft through `Effect.acquireUseRelease` or an equivalent bracket, with one release path for every exit.
- [x] 5.2 Preserve the callback's generic success, error, and requirement channels while keeping validation and commit serialized and atomic.
- [x] 5.3 Add tests proving typed action failure and validation failure close the draft, expose no partial body, release the reservation, and permit retry.
- [x] 5.4 Add tests proving callback defects and fiber interruption are preserved while cleanup runs and the same function can be built successfully afterward.
- [x] 5.5 Add or retain concurrency coverage proving a second overlapping body build fails without disturbing the active transaction.
- [x] 5.6 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test`; resolve all lifecycle failures before proceeding.

## 6. Pipeable Immutable Actor APIs

- [x] 6.1 Implement and type-test data-first and pipeable `Function.dual` overloads for `FastMath.combine` and all three `IntegerMath.with*` transformations.
- [x] 6.2 Implement and type-test data-first and pipeable `Function.dual` overloads for `MemoryAccess.withVolatile` and `withAtomic`, preserving optional defaults.
- [x] 6.3 Add runtime tests proving both call forms return equal immutable values and update examples that currently demonstrate nested transformations.
- [x] 6.4 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test`; resolve all structural-tier failures before proceeding.

## 7. Documentation, Packaging, and Release Validation

- [x] 7.1 Update the README, actor reference, Effect-native builder explanation, tutorials/how-to guides, API examples, and changelog for `LlvmError`, pipeable forms, scoped body construction, and `@effect/vitest` conventions.
- [x] 7.2 Add the breaking package changeset/release metadata and update release-candidate assertions for the removed `SilkError` subpath and added `LlvmError` subpath.
- [x] 7.3 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test` in the required order and record the actual results.
- [x] 7.4 Run `pnpm check` and fix any aggregate workspace or script-test failures.
- [x] 7.5 Run `pnpm release:candidate` because package exports and contents changed, and verify the packed artifact exposes `LlvmError`, omits `SilkError`, and contains the updated documentation.
