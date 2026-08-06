import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as Mir from '../src/Mir.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const snapshot = (
  root: string,
  sources: Readonly<Record<string, string>>,
  target?: string,
): Effect.Effect<Analysis.Snapshot> => {
  const rootText = sources[root]
  if (rootText === undefined) throw new RangeError(`Fixture has no root source ${root}`)
  const imports = new Map(
    Object.entries(sources)
      .filter(([name]) => name !== root)
      .map(([name, source]) => [name, ascii(source)] as const),
  )
  return Analysis.make(
    target === undefined
      ? { root: SourceFile.make(root, ascii(rootText)) }
      : { root: SourceFile.make(root, ascii(rootText)), target },
  ).pipe(Effect.provide(SourceResolver.memory(imports)))
}

it.effect('binds namespace aliases and selected members to canonical calls', () =>
  Effect.gen(function* () {
    const self = yield* snapshot('app/Main', {
      'app/Main': 'import compiler.Syntax as Tree\npub fn main() -> I32 { return Tree.parse() }',
      'compiler/Syntax': 'pub fn parse() -> I32 { return 42 }\nfn hidden() -> I32 { return 0 }',
    })
    const returned = Analysis.hirOf(self, 'app/Main')?.functions.at(0)
    const expression = returned === undefined ? undefined : Hir.returned(returned)
    assert.strictEqual(expression?._tag, 'Call')
    if (expression?._tag === 'Call') {
      assert.deepEqual(expression.target, {
        _tag: 'CanonicalDeclarationId',
        module: 'compiler/Syntax',
        name: 'parse',
      })
    }

    const selected = yield* snapshot('app/Main', {
      'app/Main':
        'import compiler.Syntax { parse as read, hidden }\npub fn main() -> I32 { return hidden() }',
      'compiler/Syntax': 'pub fn parse() -> I32 { return 42 }\nfn hidden() -> I32 { return 0 }',
    })
    assert.strictEqual(Analysis.lookupName(selected, 'app/Main', 'read')._tag, 'Resolved')
    assert.strictEqual(Analysis.lookupName(selected, 'app/Main', 'hidden')._tag, 'Unavailable')
    assert.deepEqual(
      Analysis.diagnostics(selected).map((diagnostic) => diagnostic.code),
      ['SEM0015'],
    )
  }),
)

it.effect('resolves imported pipelines identically across targets and repeated snapshots', () =>
  Effect.gen(function* () {
    const sources = {
      'app/Main':
        'import compiler.Syntax as Tree\npub fn main() -> I32 { return 40 |> Tree.add(2) }',
      'compiler/Syntax': 'pub fn add(left: I32, right: I32) -> I32 { return left + right }',
    }
    const first = yield* snapshot('app/Main', sources, 'aarch64-apple-darwin')
    const second = yield* snapshot('app/Main', sources, 'aarch64-apple-darwin')
    const llvmA = yield* Analysis.codegen(first, { mode: 'release' })
    const llvmB = yield* Analysis.codegen(second, { mode: 'release' })
    const wasm = yield* Analysis.codegenWasm(
      yield* snapshot('app/Main', sources, 'wasm32-unknown-unknown'),
      { mode: 'release' },
    )
    assert.deepEqual(llvmA.bitcode, llvmB.bitcode)
    assert.match(wasm.ir, /call/)
    assert.strictEqual(Analysis.evaluate(first)._tag, 'Completed')
  }),
)

it.effect('keeps cyclic cross-module calls finite and canonical', () =>
  Effect.gen(function* () {
    const self = yield* snapshot('app/Main', {
      'app/Main': 'import cycle.A { a }\npub fn main() -> I32 { return a() }',
      'cycle/A': 'import cycle.B { b }\npub fn a() -> I32 { return b() }',
      'cycle/B': 'import cycle.A { a }\npub fn b() -> I32 { return a() }',
    })
    assert.deepEqual(self.closure.cycles, [['cycle/A', 'cycle/B']])
    assert.deepEqual(
      self.instances.instances.map((instance) => instance.key.declaration.module),
      ['app/Main', 'cycle/A', 'cycle/B'],
    )
  }),
)

const threeModuleSources = {
  'app/Main': 'import library.Answer { answer }\npub fn main() -> I32 { return answer() }',
  'library/Answer':
    'import values.Number\nfn local() -> I32 { return 40 }\npub fn answer() -> I32 { return I32.add(local(), Number.two()) }',
  'values/Number': 'pub fn two() -> I32 { return 2 }',
}

it.effect('keeps HIR, MIR, diagnostics, and instances deterministic across fresh snapshots', () =>
  Effect.gen(function* () {
    const forward = yield* snapshot('app/Main', threeModuleSources)
    const reverse = yield* snapshot('app/Main', {
      'values/Number': threeModuleSources['values/Number'],
      'library/Answer': threeModuleSources['library/Answer'],
      'app/Main': threeModuleSources['app/Main'],
    })
    assert.deepEqual(
      [...forward.results.values()].map((result) => Hir.encode(result.hir)),
      [...reverse.results.values()].map((result) => Hir.encode(result.hir)),
    )
    assert.strictEqual(
      Mir.encode(Analysis.loweredMir(forward)),
      Mir.encode(Analysis.loweredMir(reverse)),
    )
    assert.deepEqual(forward.instances, reverse.instances)
    assert.deepEqual(forward.diagnostics, reverse.diagnostics)
  }),
)

it.effect('diagnoses import binding conflicts stably without parser cascades', () =>
  Effect.gen(function* () {
    const duplicate = yield* snapshot('root', {
      root: 'import library.One\nimport library.One as Again\npub fn main() -> I32 { return 0 }',
      'library/One': 'pub fn one() -> I32 { return 1 }',
    })
    const recovered = yield* snapshot('root', {
      root: 'import library.One as\npub fn main() -> I32 { return 0 }',
      'library/One': 'pub fn one() -> I32 { return 1 }',
    })
    assert.deepEqual(
      duplicate.diagnostics.map((diagnostic) => diagnostic.code),
      ['MOD0003'],
    )
    assert.deepEqual(
      recovered.diagnostics.map((diagnostic) => diagnostic.code),
      ['PAR0001'],
    )
    assert.strictEqual(Analysis.lookupName(recovered, 'root', 'One')._tag, 'Missing')
  }),
)

it.effect('discovers one shared diamond target and excludes unreachable declarations', () =>
  Effect.gen(function* () {
    const self = yield* snapshot('root', {
      root: 'import left.Api { left }\nimport right.Api { right }\npub fn main() -> I32 { return I32.add(left(), right()) }',
      'left/Api':
        'import shared.Value { value }\npub fn left() -> I32 { return value() }\npub fn unused() -> I32 { return 0 }',
      'right/Api': 'import shared.Value { value }\npub fn right() -> I32 { return value() }',
      'shared/Value': 'pub fn value() -> I32 { return 21 }',
    })
    assert.deepEqual(
      self.instances.instances.map(
        (instance) => `${instance.key.declaration.module}.${instance.key.declaration.name}`,
      ),
      ['root.main', 'left/Api.left', 'right/Api.right', 'shared/Value.value'],
    )
  }),
)
