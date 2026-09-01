import { spawnSync } from 'node:child_process'
import { createHash } from 'node:crypto'
import { existsSync, mkdtempSync, readFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as MirEncoding from '../src/MirEncoding.js'
import type * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Projections from './support/projections.js'
import * as Driver from './support/TestDriver.js'

const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/${name}`, import.meta.url), 'utf8')

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
  Analysis.makeRealized({ root: SourceFile.make(rootModule, rootBytes), target }).pipe(
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
    // `usize` renders and reads decimal text, so naming it reaches the formatting, writer, Effect,
    // and standard-service stack. Module discovery remains distinct from executable reachability:
    // codegen emits only declarations reached from the entry point.
    assert.deepEqual(
      Analysis.modules(self).map((module) => module.name),
      [
        'app/Main',
        'compiler/Coverage',
        'compiler/Member',
        'silk/allocator',
        'silk/bool',
        'silk/bytes',
        'silk/char',
        'silk/effect',
        'silk/f32',
        'silk/f64',
        'silk/format',
        'silk/i16',
        'silk/i32',
        'silk/i64',
        'silk/i8',
        'silk/isize',
        'silk/layout',
        'silk/logger',
        'silk/monotonic_clock',
        'silk/option',
        'silk/order',
        'silk/raw_buffer',
        'silk/reflect',
        'silk/result',
        'silk/slot',
        'silk/static_sequence',
        'silk/static_text',
        'silk/string',
        'silk/system_clock',
        'silk/target',
        'silk/u16',
        'silk/u32',
        'silk/u64',
        'silk/u8',
        'silk/usize',
        'silk/vector',
        'silk/writer',
      ],
    )
    for (const name of moduleNames) {
      assert.notStrictEqual(Projections.syntaxOf(self, name), undefined)
      assert.notStrictEqual(Projections.hirOf(self, name), undefined)
      assert.notStrictEqual(Analysis.ownershipOf(self, name), undefined)
    }
    assert.isAbove(Analysis.instancesOf(self).instances.length, 0)
    assert.strictEqual(Analysis.layoutOf(self)._tag, 'Available')
    const lowered = Analysis.loweredMir(self)
    assert.isAbove(lowered.functions.length, 0)
    assert.strictEqual(
      `${createHash('sha256').update(MirEncoding.encode(lowered)).digest('hex')}\n`,
      golden('algorithmic.mir.sha256'),
    )

    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 42n)
    assert.deepEqual(
      outcome.trace.flatMap((event) =>
        event._tag === 'Return' &&
        event.function.name === 'fold' &&
        event.value._tag === 'IntegerValue'
          ? [event.value.value]
          : [],
      ),
      [40n, 42n],
    )
    assert.strictEqual(
      Analysis.instancesOf(self).instances.filter(
        (instance) => instance.key.declaration.name === 'fold',
      ).length,
      1,
    )
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
    const nativeArtifact = yield* Analysis.codegen(yield* snapshot('aarch64-apple-darwin'), {
      mode: 'release',
    })
    const wasmInstance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
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
    let nativeFailure: string | undefined
    if (native._tag === 'BackendFailed') {
      const reason =
        native.error.reason._tag === 'WrappedFailure'
          ? native.error.reason.cause
          : JSON.stringify(native.error.reason)
      nativeFailure = `${native.error.message}: ${String(reason)}`
    }
    assert.strictEqual(native._tag, 'Compiled', nativeFailure)
    if (native._tag !== 'Compiled') return
    assert.strictEqual(existsSync(native.path), true)
    const nativeRun = spawnSync(native.path, [], { encoding: 'utf8' })

    assert.strictEqual(logical.result.value, 42n)
    assert.strictEqual(wasmMain(), Number(logical.result.value))
    assert.strictEqual(nativeRun.status, Number(logical.result.value), nativeRun.stderr)
    assert.strictEqual(
      nativeArtifact.symbols.filter((entry) => entry.declaration.name === 'fold').length,
      1,
    )
    assert.strictEqual(wasm.symbols.filter((entry) => entry.declaration.name === 'fold').length, 1)
  }),
)
