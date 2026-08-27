## Why

`@silklang/llvm` is architecturally Effect-native, but several public and internal paths still bypass the intended typed failure, lifecycle, composition, and authoring conventions. The repository guidance also reflects an older version of the Effect patterns policy, so the implementation and its documented standard should be aligned before the package API stabilizes.

## What Changes

- **BREAKING** Replace the generic `SilkError` public actor, class, tag, and package subpath with the LLVM-specific `LlvmError` contract, and distinguish invalid input/context from genuine causal failures.
- Ensure every expected invalid numeric input, including values converted to `bigint`, fails through the typed `LlvmError` channel rather than becoming a defect.
- Remove public synchronous throws of yieldable errors and stop using `LlvmError` as exception-based control flow inside builder transitions.
- Bracket function-body reservation, validation, commit, and cleanup with Effect lifecycle primitives so cleanup is guaranteed on failure, defect, and interruption.
- Add `Function.dual` overloads to immutable actor transformations where both data-first and pipeable forms are useful.
- Convert reusable Effect-returning helpers and intrinsic recipes to the repository's selected `Effect.fn`/`Effect.fnUntraced` authoring forms.
- Refine internal discriminated unions and lookup types so encoding and operand handling no longer require avoidable casts.
- Migrate Effect tests from the stale `ManagedRuntime`/Vitest pattern to `@effect/vitest`, `it.effect`, shared layers, and `assert`.
- Update `AGENTS.md`, package documentation, examples, exports, tests, and release metadata to make the current Effect patterns policy authoritative.

## Capabilities

### New Capabilities

- `llvm-error-model`: Defines the LLVM-specific typed error contract and guarantees that expected validation failures do not throw or die.
- `llvm-function-body-lifecycle`: Defines transactional function-body construction and cleanup behavior across success, failure, defect, and interruption.
- `llvm-data-composition`: Defines data-first and pipeable call forms for immutable LLVM actor transformations.

### Modified Capabilities

None. This repository does not yet contain main OpenSpec capability specifications.

## Impact

- Public API: `@silklang/llvm/SilkError` and `SilkError` are replaced by `@silklang/llvm/LlvmError` and `LlvmError`; immutable transformation functions gain pipeable overloads.
- Implementation: builder and function-body state transitions, numeric normalization, bitcode encoding helpers, intrinsic recipes, and internal instruction types are affected.
- Tests and tooling: `@effect/vitest` is added for package tests; the shared `ManagedRuntime` test harnesses are removed.
- Documentation and release: actor references, examples, package exports, changelog/changeset metadata, repository conventions, and release-candidate validation must reflect the new contract.
