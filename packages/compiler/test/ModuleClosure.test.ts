import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Exit from 'effect/Exit'
import * as Layer from 'effect/Layer'
import * as Option from 'effect/Option'
import * as ModuleClosure from '../src/ModuleClosure.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceOrigin from '../src/SourceOrigin.js'
import * as SourceResolver from '../src/SourceResolver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const fn = 'pub fn main() -> i32 { return 42 }'

const fixture = (
  rootModule: string,
  entries: ReadonlyArray<readonly [string, string]>,
): Effect.Effect<ModuleClosure.Closure> => {
  const rootText = entries.find(([name]) => name === rootModule)?.[1]
  if (rootText === undefined) throw new RangeError(`Fixture has no root source ${rootModule}`)
  const imports = new Map(
    entries
      .filter(([name]) => name !== rootModule)
      .map(([name, text]) => [name, ascii(text)] as const),
  )
  return ModuleClosure.load({ root: SourceFile.make(rootModule, ascii(rootText)) }).pipe(
    Effect.provide(SourceResolver.memory(imports)),
  )
}

const importNames = (module: ModuleClosure.Module): ReadonlyArray<string> =>
  module.imports.map((fact) =>
    fact.target._tag === 'Unavailable' ? '<unavailable>' : fact.target.module,
  )

it.effect('loads a diamond once per module and excludes unreachable sources', () =>
  Effect.gen(function* () {
    const calls: Array<string> = []
    const sources = new Map([
      ['left', ascii(`import shared\n${fn}`)],
      ['right', ascii(`import shared\n${fn}`)],
      ['shared', ascii(fn)],
      ['island', ascii(fn)],
    ])
    const resolver = Layer.succeed(SourceResolver.SourceResolver, {
      resolveStandardLibrary: SourceResolver.resolveEmbeddedStandardLibrary,
      toolchainSources: SourceResolver.embeddedToolchainSources,
      resolve: (module: string) =>
        Effect.sync(() => {
          calls.push(module)
          const bytes = sources.get(module)
          return bytes === undefined
            ? Option.none()
            : Option.some(SourceResolver.resolved(bytes, SourceOrigin.memory()))
        }),
    })
    const closure = yield* ModuleClosure.load({
      root: SourceFile.make('root', ascii(`import left\nimport right\n${fn}`)),
    }).pipe(Effect.provide(resolver))

    assert.deepEqual(
      closure.modules.map((module) => module.name),
      ['left', 'right', 'root', 'shared'],
    )
    assert.deepEqual(calls, ['left', 'right', 'shared'])
    assert.deepEqual([...closure.sources.keys()], ['left', 'right', 'root', 'shared'])
    assert.deepEqual(closure.resolutionFailures, [])
    assert.strictEqual(Object.isFrozen(closure), true)
  }),
)

it.effect('is deterministic across resolver supply order', () =>
  Effect.gen(function* () {
    const entries: ReadonlyArray<readonly [string, string]> = [
      ['root', `import zeta\nimport alpha\n${fn}`],
      ['zeta', `import alpha\n${fn}`],
      ['alpha', fn],
    ]
    const forward = yield* fixture('root', entries)
    const reversed = yield* fixture('root', [...entries].reverse())
    assert.deepEqual(forward, reversed)
  }),
)

it.effect('diagnoses absence and self-imports without resolving the root', () =>
  Effect.gen(function* () {
    const closure = yield* fixture('root', [['root', `import missing\nimport root\n${fn}`]])
    const root = closure.modules.at(0)
    assert.notStrictEqual(root, undefined)
    if (root === undefined) return
    assert.deepEqual(importNames(root), ['missing', 'root'])
    assert.deepEqual(
      closure.diagnostics.map((diagnostic) => diagnostic.code),
      ['MOD0001', 'MOD0002'],
    )
    assert.strictEqual(root.imports.at(0)?.target._tag, 'Unknown')
    assert.strictEqual(root.imports.at(1)?.target._tag, 'Self')
  }),
)

it.effect('suppresses resolver calls and module diagnostics for damaged import syntax', () =>
  Effect.gen(function* () {
    const closure = yield* fixture('root', [['root', `import\n${fn}`]])
    const root = closure.modules.at(0)
    assert.strictEqual(root?.imports.at(0)?.target._tag, 'Unavailable')
    assert.deepEqual(closure.diagnostics, [])
    assert.deepEqual(
      root?.syntax.parserDiagnostics.map((diagnostic) => diagnostic.code),
      ['PAR0001'],
    )
  }),
)

it.effect('retains partial closure facts around ordered operational failures', () =>
  Effect.gen(function* () {
    const resolver = Layer.succeed(SourceResolver.SourceResolver, {
      resolveStandardLibrary: SourceResolver.resolveEmbeddedStandardLibrary,
      toolchainSources: SourceResolver.embeddedToolchainSources,
      resolve: (module: string) => {
        if (module === 'readable') {
          return Effect.succeed(
            Option.some(SourceResolver.resolved(ascii(fn), SourceOrigin.memory())),
          )
        }
        return Effect.fail(
          new SourceResolver.SourceResolverError({
            operation: 'test.resolve',
            module,
            message: `cannot read ${module}`,
            reason: { _tag: 'WrappedFailure', cause: new Error(module) },
          }),
        )
      },
    })
    const closure = yield* ModuleClosure.load({
      root: SourceFile.make('root', ascii(`import zeta\nimport readable\nimport alpha\n${fn}`)),
    }).pipe(Effect.provide(resolver))

    assert.deepEqual(
      closure.modules.map((module) => module.name),
      ['readable', 'root'],
    )
    assert.deepEqual(
      closure.resolutionFailures.map((failure) => failure.module),
      ['alpha', 'zeta'],
    )
    assert.deepEqual(closure.diagnostics, [])
    assert.deepEqual(
      closure.modules
        .find((module) => module.name === 'root')
        ?.imports.map((fact) => fact.target._tag),
      ['Failed', 'Resolved', 'Failed'],
    )
  }),
)

it.effect('records cycles and exact dotted-to-canonical import provenance', () =>
  Effect.gen(function* () {
    const closure = yield* fixture('app/Main', [
      ['app/Main', `import compiler.Syntax as Tree { parse }\n${fn}`],
      ['compiler/Syntax', `import cycle.Other\npub fn parse() -> i32 { return 42 }`],
      ['cycle/Other', `import compiler.Syntax\n${fn}`],
    ])
    const imported = closure.modules.find((module) => module.name === 'app/Main')?.imports.at(0)
    assert.strictEqual(imported?.sourceSpelling, 'compiler.Syntax')
    assert.strictEqual(imported?.canonicalTarget, 'compiler/Syntax')
    assert.strictEqual(imported?.target._tag, 'Resolved')
    assert.deepEqual(closure.cycles, [['compiler/Syntax', 'cycle/Other']])
  }),
)

it.effect('maps reserved contextual path segments to canonical module identities', () =>
  Effect.gen(function* () {
    const closure = yield* fixture('app/Main', [
      ['app/Main', `import toolkit.effect.helpers as Helpers\n${fn}`],
      ['toolkit/effect/helpers', 'pub fn answer() -> i32 { return 42 }'],
    ])
    const imported = closure.modules.find((module) => module.name === 'app/Main')?.imports.at(0)
    assert.strictEqual(imported?.sourceSpelling, 'toolkit.effect.helpers')
    assert.strictEqual(imported?.canonicalTarget, 'toolkit/effect/helpers')
    assert.strictEqual(imported?.target._tag, 'Resolved')
    assert.deepEqual(closure.diagnostics, [])
  }),
)

it.effect('preserves case and rejects malformed explicit root identities', () =>
  Effect.gen(function* () {
    const mismatch = yield* fixture('app/Main', [
      ['app/Main', `import compiler.Syntax\n${fn}`],
      ['compiler/syntax', fn],
    ])
    assert.deepEqual(
      mismatch.diagnostics.map((diagnostic) => diagnostic.code),
      ['MOD0001'],
    )
    for (const invalid of [
      '/absolute',
      'with.ext',
      'empty//segment',
      'dot/../segment',
      'scheme://module',
    ]) {
      const exit = yield* Effect.exit(
        ModuleClosure.load({ root: SourceFile.make(invalid, ascii(fn)) }).pipe(
          Effect.provide(SourceResolver.empty),
        ),
      )
      assert.strictEqual(Exit.isFailure(exit), true)
    }
  }),
)
