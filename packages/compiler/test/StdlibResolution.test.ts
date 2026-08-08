import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as ModuleClosure from '../src/ModuleClosure.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Stdlib from '../src/Stdlib.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const importing = `import silk.vector { Vector, length }

pub fn main() -> I32 {
  return 42
}`

it.effect('resolves standard-library imports without vendoring source', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource('stdlib/importer', ascii(importing))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(
      Analysis.modules(snapshot).map((module) => module.name),
      ['silk/vector', 'stdlib/importer'].sort(),
    )
    // The resolved declarations carry the library's canonical module identity.
    const index = Analysis.declarationIndex(snapshot)
    const library = index.modules.find((module) => module.module === 'silk/vector')
    assert.isDefined(library)
    assert.isTrue(
      library !== undefined &&
        library.structs.some(
          (struct) =>
            struct.canonical._tag === 'Canonical' && struct.canonical.id.name === 'Vector',
        ),
    )
  }),
)

it.effect('never consults a user resolver inside the reserved namespace', () =>
  Effect.gen(function* () {
    // A user resolver claims to supply silk/vector with hostile bytes; the closure must take
    // the compiler-shipped source instead.
    const hostile = new Map([['silk/vector', ascii('pub fn stolen() -> I32 { return 0 }')]])
    const closure = yield* ModuleClosure.load({
      root: SourceFile.make('stdlib/hostile-importer', ascii(importing)),
    }).pipe(Effect.provide(SourceResolver.memory(hostile)))
    const library = closure.modules.find((module) => module.name === 'silk/vector')
    assert.isDefined(library)
    const librarySource = closure.sources.get('silk/vector')
    assert.isTrue(
      librarySource !== undefined &&
        new TextDecoder()
          .decode(SourceFile.toUint8Array(librarySource))
          .includes('struct Vector<T>'),
    )
  }),
)

it.effect('rejects a user root claiming the reserved namespace', () =>
  Effect.gen(function* () {
    const closure = yield* ModuleClosure.load({
      root: SourceFile.make('silk/impostor', ascii('pub fn main() -> I32 { return 0 }')),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.include(
      closure.diagnostics.map((diagnostic) => diagnostic.code),
      'MOD0004',
    )
  }),
)

it.effect('compiles library source with ordinary diagnostics and no privilege', () =>
  Effect.gen(function* () {
    // Inject a defect into the shipped library table for this test only: it must surface as an
    // ordinary semantic diagnostic attributed to the library module, exactly like user code.
    const sources = Stdlib.sources as Map<string, Uint8Array>
    const original = sources.get('silk/vector')
    assert.isDefined(original)
    if (original === undefined) return
    sources.set('silk/vector', ascii('pub fn broken() -> Missing { return 0 }'))
    try {
      const snapshot = yield* Analysis.ofSource('stdlib/defective-importer', ascii(importing))
      const attributed = Analysis.diagnostics(snapshot).filter(
        (diagnostic) => diagnostic.span.sourceId === 'silk/vector',
      )
      assert.isAbove(attributed.length, 0)
      assert.include(
        attributed.map((diagnostic) => diagnostic.code),
        'SEM0001',
      )
    } finally {
      sources.set('silk/vector', original)
    }
  }),
)

it('keeps stdlib-importing artifacts byte-identical across fresh processes', () => {
  const fixture = fileURLToPath(new URL('./fixtures/stdlib-determinism.mjs', import.meta.url))
  const run = () => spawnSync(process.execPath, [fixture], { encoding: 'utf8' })
  const first = run()
  const second = run()

  assert.strictEqual(first.status, 0, first.stderr)
  assert.strictEqual(second.status, 0, second.stderr)
  assert.strictEqual(first.stdout, second.stdout)
  const encoded = JSON.parse(first.stdout) as {
    readonly diagnostics: ReadonlyArray<unknown>
    readonly modules: ReadonlyArray<string>
  }
  assert.deepEqual(encoded.diagnostics, [])
  assert.include(encoded.modules, 'silk/vector')
})
