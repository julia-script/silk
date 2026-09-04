import { createHash } from 'node:crypto'
import { readFileSync } from 'node:fs'
import { join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Projections from './support/projections.js'

const fixtureRoot = fileURLToPath(new URL('./fixtures/algorithmic-acceptance', import.meta.url))
const rootModule = 'app/Main'
const moduleNames = ['app/Main', 'compiler/Coverage', 'compiler/Member'] as const
const modules = new Map(
  moduleNames.map((name) => [
    name,
    new Uint8Array(readFileSync(join(fixtureRoot, `${name}.silk`))),
  ]),
)
const rootBytes = modules.get(rootModule)
if (rootBytes === undefined) throw new RangeError(`Fixture has no root module ${rootModule}`)

it.effect('accepts the compiler-shaped fold through every static compiler phase', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.makeRealized({
      root: SourceFile.make(rootModule, rootBytes),
      target: 'aarch64-apple-darwin',
    }).pipe(
      Effect.provide(
        SourceResolver.memory(new Map([...modules].filter(([name]) => name !== rootModule))),
      ),
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    for (const name of moduleNames) {
      assert.isDefined(Projections.syntaxOf(self, name))
      assert.isDefined(Projections.hirOf(self, name))
      assert.isDefined(Analysis.ownershipOf(self, name))
    }
    assert.isAbove(Analysis.instancesOf(self).instances.length, 0)
    assert.strictEqual(Analysis.layoutOf(self)._tag, 'Available')
    const lowered = Analysis.loweredMir(self)
    assert.isAbove(lowered.functions.length, 0)
    assert.strictEqual(
      Analysis.instancesOf(self).instances.filter(
        (instance) => instance.key.declaration.name === 'fold',
      ).length,
      1,
    )
    const expected = readFileSync(
      new URL('./goldens/algorithmic.mir.sha256', import.meta.url),
      'utf8',
    )
    assert.strictEqual(
      `${createHash('sha256').update(MirEncoding.encode(lowered)).digest('hex')}\n`,
      expected,
    )
    const llvm = yield* Analysis.codegen(self, { mode: 'release' })
    assert.strictEqual(llvm.symbols.filter((entry) => entry.declaration.name === 'fold').length, 1)
  }),
)
