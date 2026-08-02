## Context

See `proposal.md` for motivation. The package is a strict TypeScript library whose public API is organized into actor modules and explicit package subpaths. Builder state is mutable but hidden behind a semaphore and opaque handles; bitcode and text output are synchronous in-memory algorithms exposed through Effect boundaries. The current tests use a repository-specific `ManagedRuntime` pattern that has been superseded by the authoritative Effect patterns policy.

## Goals / Non-Goals

**Goals:**

- Preserve the actor-oriented public surface while making every expected failure, transaction lifetime, and reusable Effect function conform to the current Effect policy.
- Make the breaking error rename complete and mechanically discoverable rather than retaining two competing public names.
- Keep builder transitions synchronous and serialized even after removing exception-based typed-error control flow.
- Add focused regression coverage for defects, interruption cleanup, both dual call forms, and public export behavior.

**Non-Goals:**

- Introducing one error class per LLVM actor or operation without a demonstrated recovery distinction.
- Replacing the in-memory LLVM encoder with native LLVM bindings, filesystem access, or subprocess execution.
- Splitting coherent large actors such as `FunctionBody`, `Metadata`, or `Bitcode` solely because of file size.
- Reworking LLVM parity behavior or adding new IR features.

## Decisions

### 1. Replace `SilkError` with one LLVM-specific family and semantic reasons

Rename the actor, class, Effect tag, source file, root export, and package subpath to `LlvmError`. Do not retain a deprecated `SilkError` alias: the package is pre-stable, the user requested an LLVM-specific contract, and an alias would allow stale imports and `catchTag` branches to persist silently.

Keep one error family because current consumers recover by the common tag and inspect `operation`/`message`; there is no evidence that per-actor error classes improve recovery. Add a small discriminated `reason` model for invalid input, invalid state/ownership, and wrapped failure. Only the wrapped-failure reason supplies JavaScript causal ancestry; validation values remain semantic data rather than `cause`.

Alternatives considered: keeping `SilkError` as an alias was rejected because it weakens the migration; creating dozens of actor-specific errors was rejected because callers have no corresponding recovery branches today.

### 2. Normalize integer inputs before calling `BigInt`

Centralize `number | bigint` normalization in a coherent internal integer-input actor. It validates finite safe integer numbers before conversion and applies operation-specific range checks in the typed channel. Callers such as alignment, attributes, constants, types, and metadata reuse it so native `RangeError` cannot become a defect.

Alternatives considered: wrapping every `BigInt` call independently with `Effect.try` would work but duplicate subtle validation and diagnostics across actors.

### 3. Make synchronous state transitions return typed results

Change builder and function-body mutation callbacks from “return a value or throw `LlvmError`” to a synchronous typed-result contract, using Effect's `Result` representation. The semaphore owner evaluates the transition synchronously and converts its result into the Effect channel. Unexpected JavaScript throws remain defects; expected validation uses `Result.fail`.

This retains the important property that mutable state is never held across an asynchronous Effect while removing yieldable errors from exception control flow. Introduce a narrowly named internal transition actor only if it materially reduces repetitive `Result` construction; do not create a generic helpers module.

For private bitstream and rendering loops, use private non-yieldable implementation failures where a synchronous encoder must abort, then translate once in `Bitcode.encode` or `IrText.render`. The documented measured imperative loops remain intact.

Alternatives considered: effectful mutation callbacks were rejected because they could suspend while holding the builder permit; retaining thrown `LlvmError` was rejected by the authoritative error policy.

### 4. Scope function-body reservation with an Effect bracket

Refactor `Function.buildBody` around `Effect.acquireUseRelease` or an equivalent scoped bracket. Acquisition reserves the function and creates the draft. Use runs the action, validates, and commits. The release phase always closes the draft and removes any outstanding reservation, using the use exit to select `committed` or `failed` state without replacing the original failure, defect, or interruption.

Commit remains serialized by the builder semaphore, and the action's generic error and requirement channels remain unchanged. Add explicit interruption and defect tests that retry the same function afterward.

Alternatives considered: consolidating the existing repeated branches into a local cleanup function still relies on every exit path invoking it and does not provide the same lifecycle guarantee.

### 5. Make fallible alignment encoding effectful at the public boundary

Change the public `MemoryAccess.alignmentCode` API to return an Effect with `LlvmError`. Keep synchronous encoder internals private: they may consume a synchronous typed result or use a private implementation failure that the outer encoding boundary translates. No public yieldable error is thrown.

### 6. Add dual overloads only to genuine actor transformations

Use `effect/Function.dual` for `FastMath.combine`, `IntegerMath.withNoSignedWrap`, `withNoUnsignedWrap`, `withExact`, `MemoryAccess.withVolatile`, and `withAtomic`. Preserve current data-first order and defaults. Queries and renderers remain data-first unless a concrete pipeline use makes the curried form useful.

### 7. Use Effect-native reusable function forms

Keep named `Effect.fn('Actor.operation')` on public actor operations because those names are existing observability boundaries. Convert small internal reusable functions and intrinsic recipe callbacks to `Effect.fnUntraced`; reserve inline `Effect.gen` for one-off composition. Forwarding functions such as `Metadata.emptyTuple` also receive an Effect-native function definition rather than returning an Effect from a plain arrow.

### 8. Encode internal relationships in types instead of casts

Split compare instructions into integer and floating discriminated members so `kind` determines the predicate type. Narrow value/constant inputs by their handle tag, and type intrinsic recipe tables as partial records keyed by intrinsic id. Retain only `as const` and casts that TypeScript genuinely cannot express.

### 9. Adopt `@effect/vitest` and update the repository policy

Add `@effect/vitest` as a development dependency and migrate Effect tests to `it.effect`, `it.layer` where a shared layer exists, and `assert`. Pure synchronous bitstream and byte-string cases remain ordinary tests rather than manufacturing Effects. Remove per-file `ManagedRuntime` instances and update `AGENTS.md` so the current Effect patterns skill governs error semantics, function authoring, testing, and lifecycle guidance.

## Risks / Trade-offs

- [Breaking error import and tag rename] → Update every source import, test, TSDoc example, package export, actor reference, changelog entry, and release-candidate assertion in one change; do not ship a partial alias-based migration.
- [Large mechanical transition migration] → Convert one boundary at a time, typecheck after each actor group, and keep synchronous transitions under the semaphore.
- [Error reason model becomes over-specific] → Limit reasons to distinctions already needed by the policy—input, state/ownership, and causal failure—and defer actor-specific errors until consumers need different recovery.
- [Bracket refactor changes generic inference] → Pin `Function.buildBody` with `Effect.fn.Return` and add compile-time coverage for action success, error, and requirement channels.
- [Dual overloads alter inference or defaults] → Preserve explicit overload signatures and test both invocation forms, including omitted optional arguments.
- [Test migration obscures behavioral regressions] → Migrate assertions without changing fixtures or expected outputs, then run the full parity suite and release candidate.

## Migration Plan

1. Introduce `LlvmError` and update all internal references and semantic error construction before removing `SilkError` exports.
2. Repair numeric normalization and synchronous transition contracts, then make the public alignment encoder effectful.
3. Bracket function-body construction and add failure, defect, interruption, and retry coverage.
4. Add dual overloads, repair internal discriminated types, and convert reusable Effect helpers.
5. Migrate tests and repository conventions, update documentation and package exports, and add release metadata.
6. Run typecheck, Biome, tests/parity validation, the repository aggregate check, and release-candidate validation.

Rollback is a single-change revert before publication. After publication, consumers must migrate imports and `catchTag` names back only by selecting an earlier package version.
