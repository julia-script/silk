# Silk Effect agent instructions

These conventions apply to the entire repository. `@silk-effect/llvm` is currently blank; new
code must establish these shapes from the first exported feature instead of adding Effect as a
wrapper after an imperative implementation already exists.

## Repository workflow

- The workspace uses pnpm, Turbo, strict TypeScript, Biome, and Vitest.
- Put public LLVM code in `packages/llvm/src` and tests in `packages/llvm/test`.
- Keep the public barrel at `packages/llvm/src/index.ts` explicit.
- Verify changes in this order: `pnpm typecheck`, `pnpm exec biome check .`, then `pnpm test`.
- Run `pnpm check` before handoff. Run `pnpm release:candidate` when package contents or exports
  change.
- Do not describe changes as complete when a required check was not run. Report the exact failure
  and whether it predates the change.

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

Use one repo-wide tagged error by default. The default is `SilkError`; introduce another tag only
when callers need to branch on that distinction.

```ts
import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'

export class SilkError extends Data.TaggedError('SilkError')<{
  readonly message: string
  readonly cause: unknown
}> {}

export const fromExternal = Effect.fn('Llvm.fromExternal')(function* (input: string) {
  return yield* Effect.try({
    try: () => externalApi(input),
    catch: (cause) => new SilkError({ message: `LLVM operation failed for ${input}`, cause }),
  })
})
```

Never throw ordinary errors across a public boundary, swallow errors in `catch`, or expose
`unknown` as a public Effect error channel.

## Effectful functions are Effect.fn

Define effectful functions with `Effect.fn('Actor.operation')`. Use `Effect.fnUntraced` only for a
measured hot path or a small internal function where span overhead matters. Do not define an arrow
that merely returns `Effect.gen`; that drops function tracing, span arguments, and improved stack
traces.

```ts
export const compile = Effect.fn('Compiler.compile')(function* (
  target: Target.Target,
): Effect.fn.Return<Artifact.Artifact, SilkError, CompilerService> {
  const compiler = yield* CompilerService
  return yield* compiler.compile(target)
})
```

The explicit `Effect.fn.Return<A, E, R>` annotation is optional for internal functions. Use it to
pin public and recursive signatures.

Raw imperative code is allowed only inside a documented performance-critical inner loop, such as
per-instruction or per-byte processing. Keep that loop behind an effectful API, keep construction
and teardown in Effect, and add a comment naming the measured reason for the exception. A claim
that an entire package is a hot path is not an exception.

## Tests stay inside Effect

Build shared test layers once per file, create one `ManagedRuntime`, and pass its runner to
`Effect.fnUntraced`. Keep test bodies as generators that yield Effects.

```ts
import * as Effect from 'effect/Effect'
import * as Layer from 'effect/Layer'
import * as ManagedRuntime from 'effect/ManagedRuntime'
import { expect, test } from 'vitest'

const TestRuntime = ManagedRuntime.make(Layer.merge(TestCompiler.layer, Layer.empty))

test(
  'compiles a target',
  Effect.fnUntraced(function* () {
    const artifact = yield* Compiler.compile(Target.make('wasm32'))
    expect(artifact.target).toBe('wasm32')
  }, TestRuntime.runPromise),
)
```

Do not call `Effect.runPromise` or `Effect.runSync` inside each test, rebuild common layers per
test, or wrap Effect code in `async` test callbacks. A test that genuinely needs isolation may
scope a distinct layer within that test.

## Stay type-safe

- Never use non-null assertions (`!`), including in tests.
- Use `test/support/raise.ts` when a test invariant makes a nullable value impossible:
  `values.at(-1) ?? unreachable('expected a value')`.
- Use casts only for truths TypeScript cannot express, such as a conditional return type or a
  variance gap inside a generic combinator. Fix signatures instead of casting call sites.
- Never add lint suppressions to permit a cast or non-null assertion.
- Keep public Effect error and requirement channels precise; do not erase them to `unknown` or
  `never` for convenience.
