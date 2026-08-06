import { spawnSync } from 'node:child_process'
import { existsSync, mkdtempSync, readFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import type * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

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

const imports = new Map([...modules].filter(([name]) => name !== rootModule))
const resolver = SourceResolver.memory(imports)

const snapshot = (target: string) =>
  Analysis.make({ root: SourceFile.make(rootModule, rootBytes), target }).pipe(
    Effect.provide(resolver),
  )

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-algorithmic-acceptance-'))
afterAll(() => {
  rmSync(destinationRoot, { recursive: true, force: true })
})

it.effect('accepts the compiler-shaped fold through every compiler phase', () =>
  Effect.gen(function* () {
    const self = yield* snapshot('aarch64-apple-darwin')

    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.deepEqual(
      Analysis.modules(self).map((module) => module.name),
      ['app/Main', 'compiler/Coverage', 'compiler/Member'],
    )
    for (const name of moduleNames) {
      assert.notStrictEqual(Analysis.syntaxOf(self, name), undefined)
      assert.notStrictEqual(Analysis.hirOf(self, name), undefined)
      assert.notStrictEqual(Analysis.ownershipOf(self, name), undefined)
    }
    assert.isAbove(Analysis.instancesOf(self).instances.length, 0)
    assert.strictEqual(Analysis.layoutOf(self)._tag, 'Available')
    assert.isAbove(Analysis.loweredMir(self).functions.length, 0)

    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 42)
    assert.isTrue(
      outcome.trace.some((event) => event._tag === 'MatchSelected' && event.arm === 0),
      'expected the guarded First arm to be selected once',
    )
  }),
)

it.effect('keeps logical native and WebAssembly execution in parity', () =>
  Effect.gen(function* () {
    const logical = Analysis.evaluate(yield* snapshot('aarch64-apple-darwin'))
    assert.strictEqual(logical._tag, 'Completed')
    if (logical._tag !== 'Completed') return

    const wasm = yield* Analysis.codegenWasm(yield* snapshot('wasm32-unknown-unknown'), {
      mode: 'release',
    })
    const wasmInstance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bitcode.slice()), {})
    const wasmMain = wasmInstance.exports.silk_main as () => number

    const toolchain: NativeToolchain.Toolchain = Object.freeze({
      _tag: 'Toolchain',
      clang: '/usr/bin/clang',
    })
    const native = yield* Driver.compile({
      compilation: { root: SourceFile.make(rootModule, rootBytes) },
      toolchain,
      profile: 'release',
      destination: join(destinationRoot, 'coverage-fold'),
    }).pipe(Effect.provide(resolver))
    assert.strictEqual(
      native._tag,
      'Compiled',
      native._tag === 'BackendFailed'
        ? `${native.error.message}: ${String(
            native.error.reason._tag === 'WrappedFailure'
              ? native.error.reason.cause
              : JSON.stringify(native.error.reason),
          )}`
        : undefined,
    )
    if (native._tag !== 'Compiled') return
    assert.strictEqual(existsSync(native.executable), true)
    const nativeRun = spawnSync(native.executable, [], { encoding: 'utf8' })

    assert.strictEqual(logical.result.value, 42)
    assert.strictEqual(wasmMain(), logical.result.value)
    assert.strictEqual(nativeRun.status, logical.result.value, nativeRun.stderr)
  }),
)
