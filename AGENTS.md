# Silk Effect agent instructions

## Project stage

This project is either unreleased or undergoing a quiet alpha review. Attempts to preserve backward
compatibility are forbidden at this stage. Do not create technical debt or compromise a better
implementation to remain compatible with the current API. Breaking any API is not only allowed but
encouraged when it advances the project toward an eventual stable version we can be confident in.

These conventions apply to the entire repository. The current `effect-patterns` skill is the
authoritative source for Effect architecture. When this file and that skill differ, follow the
skill and update this file rather than preserving an older convention.

## Agent skills

### Issue tracker

Use the local Markdown tracker. General issues live under `.scratch/<feature>/`; Wayfinder maps
live under visible `wayfinder/<effort>/` directories. See `docs/agents/issue-tracker.md`.


## Repository workflow

- The workspace uses pnpm, Turbo, strict TypeScript, Biome, and Vitest.
- Put public LLVM code in `packages/llvm/src` and tests in `packages/llvm/test`.
- Keep the public barrel at `packages/llvm/src/index.ts` explicit.
- Verify changes in this order: `pnpm typecheck`, `pnpm exec biome check .`, then `pnpm test`.
- Run `pnpm check` before handoff. Run `pnpm release:candidate` when package contents or exports
  change.
- Do not describe changes as complete when a required check was not run. Report the exact failure
  and whether it predates the change.

## Collaborative decision sessions

When a task requires a multi-decision interview, grilling session, or other branching design
process, publish a visible Codex task plan at the start. Show the major decision branches, mark
exactly one current branch in progress, and keep completed and pending branches visible so the user
can orient themselves. Treat the plan as a live map: update, split, reorder, add, or remove branches
as answers expose new dependencies or invalidate earlier assumptions.

Before every user-facing decision question in that session, also include a compact, friendly
Markdown checklist showing the overall session state. Use `✅` for completed branches, `🟡` for the
single current branch, and `⬜` for pending branches. Keep it pleasant and quickly scannable; group
items only when the map becomes unwieldy, and never omit the current branch or remaining work. Keep
this in-message checklist synchronized with the visible Codex task plan.

## One module per actor

Organize Effect code by actor, not by kind of implementation. An actor is a module named after one
concept, such as `Target.ts`, `Module.ts`, or `Compiler.ts`. It contains:

- the data or service named after the module; and
- sibling functions operating on that value, with the value as their first parameter.

The main export is mostly data. Data types carry no core methods, while services may expose a few
getters. Put behavior in sibling functions so the API remains composable and tree-shakeable. Use
`dual` from `effect/Function` when both data-first and pipeable call forms are useful.

```ts
import * as Function from 'effect/Function'

export interface Target {
  readonly triple: string
}

export const make = (triple: string): Target => ({ triple })

export const matches = Function.dual<
  (triple: string) => (self: Target) => boolean,
  (self: Target, triple: string) => boolean
>(2, (self, triple) => self.triple === triple)
```

Do not create class-per-entity designs, `utils.ts` or `helpers.ts` grab bags, or modules whose
exports do not orbit one concept. A new concept gets a new actor module.

Re-export public actors as namespaces from `packages/llvm/src/index.ts`:

```ts
export * as Target from './Target.js'
```

Prefer one namespace import per actor. Within the package, import the actor module directly. For a
public actor, add its explicit package subpath export and prefer the deep import:

```ts
import * as Effect from 'effect/Effect'
import * as Target from '@silk-effect/llvm/Target'
```

Avoid a growing destructured import from the package barrel.

## Wrap external APIs in Effect

Nothing that can throw or return a bare `Promise` crosses an external boundary unwrapped. Each
external dependency has one owning boundary actor. That actor converts failures to the typed error
channel and resources to a `Scope`; everything inward of the boundary stays effectful.

- Prefer Effect's built-in service modules. Use Effect's HTTP client instead of raw `fetch`, and
  Effect's filesystem service instead of `node:fs/promises`. Provide platform layers once at the
  application edge so tests can replace services without mocking globals.
- For an API with no Effect integration, wrap synchronous calls with `Effect.try` and promises with
  `Effect.tryPromise`.
- Wrap acquire/release pairs with `Effect.acquireRelease`. Do not use manual `try/finally`, expose
  `dispose()` bookkeeping, or scatter `new ExternalThing()` across modules.
- Keep boundary wrappers thin. Do not build a large imperative core and bolt Effect onto its outer
  constructor or entry point.

Use the LLVM-specific `LlvmError` family for expected package failures. Preserve the operation and
message, and distinguish invalid input, invalid state or ownership, and wrapped external failure
with a discriminated reason. Only wrapped failures carry JavaScript causal ancestry; rejected
values belong in semantic error details. Introduce another public tag only when callers need a
distinct recovery branch.

```ts
import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'

export class LlvmError extends Data.TaggedError('LlvmError')<{
  readonly operation: string
  readonly message: string
  readonly reason: {
    readonly _tag: 'WrappedFailure'
    readonly cause: unknown
  }
}> {}

export const fromExternal = Effect.fn('Llvm.fromExternal')(function* (input: string) {
  return yield* Effect.try({
    try: () => externalApi(input),
    catch: (cause) =>
      new LlvmError({
        operation: 'Llvm.fromExternal',
        message: `LLVM operation failed for ${input}`,
        reason: { _tag: 'WrappedFailure', cause },
      }),
  })
})
```

never throw yieldable errors across a public boundary, use a tagged error as synchronous control
flow, swallow errors in `catch`, or expose `unknown` as a public Effect error channel. Synchronous
mutable transitions return a typed `Result`; unexpected JavaScript throws remain
defects. Public fallible helpers return Effects. Private synchronous encoders may abort with a
private non-yieldable implementation failure that is translated once at the outer Effect boundary.

## Effectful functions are Effect.fn

Define named public actor operations with `Effect.fn('Actor.operation')`; these are the package's
observability boundaries. Define reusable internal Effect-returning functions and recipe callbacks
with `Effect.fnUntraced`. Keep inline `Effect.gen` for one-off composition rather than reusable
arrows that merely return a generator.

```ts
export const compile = Effect.fn('Compiler.compile')(function* (
  target: Target.Target,
): Effect.fn.Return<Artifact.Artifact, LlvmError, CompilerService> {
  const compiler = yield* CompilerService
  return yield* compiler.compile(target)
})
```

The explicit `Effect.fn.Return<A, E, R>` annotation is optional for internal functions. Use it to
pin public and recursive signatures. Prefer `Function.dual` for immutable actor transformations
that are useful both data-first and in a pipe; preserve the existing data-first argument order and
defaults.

Raw imperative code is allowed only inside a documented performance-critical inner loop, such as
per-instruction or per-byte processing. Keep that loop behind an effectful API, keep construction
and teardown in Effect, and add a comment naming the measured reason for the exception. A claim
that an entire package is a hot path is not an exception.

## Tests use @effect/vitest

Import `it` and `assert` from `@effect/vitest`. Use ordinary `it` for synchronous tests and
`it.effect` for Effect-returning tests. Use `it.layer` only when tests genuinely share a service
graph. Assertions stay inside the Effect generator.

```ts
import * as Effect from 'effect/Effect'
import { assert, it } from '@effect/vitest'

it.effect('compiles a target', () =>
  Effect.gen(function* () {
    const artifact = yield* Compiler.compile(Target.make('wasm32'))
    assert.strictEqual(artifact.target, 'wasm32')
  }),
)
```

Do not build a `ManagedRuntime` test harness, call `Effect.runPromise` or `Effect.runSync` inside
each test, rebuild common layers per test, or wrap Effect code in `async` callbacks. A test that
genuinely needs isolation may scope a distinct layer within that test.

## Scope resource lifecycles

Use `Effect.acquireRelease`, `Effect.acquireUseRelease`, or an equivalent scoped bracket whenever
an operation acquires a reservation, draft, handle, or external resource. Release must run after
success, typed failure, defect, and interruption without replacing the original exit. Do not rely
on duplicated cleanup branches or manual `try/finally`. Preserve generic success, error, and
requirement channels through the bracket.

## Stay type-safe

- never use non-null assertions (`!`), including in tests.
- Use `test/support/raise.ts` when a test invariant makes a nullable value impossible:
  `values.at(-1) ?? unreachable('expected a value')`.
- Use casts only for truths TypeScript cannot express, such as a conditional return type or a
  variance gap inside a generic combinator. Fix signatures instead of casting call sites.
- never add lint suppressions to permit a cast or non-null assertion.
- Keep public Effect error and requirement channels precise; do not erase them to `unknown` or
  `never` for convenience.
