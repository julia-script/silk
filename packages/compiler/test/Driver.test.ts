import { spawnSync } from 'node:child_process'
import { existsSync, mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Layer from 'effect/Layer'
import * as Analysis from '../src/Analysis.js'
import * as Backend from '../src/Backend.js'
import * as Driver from '../src/Driver.js'
import type * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import { corpus } from './support/corpus.js'

const clang = '/usr/bin/clang'
const toolchain: NativeToolchain.Toolchain = Object.freeze({ _tag: 'Toolchain', clang })

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-driver-test-'))
afterAll(() => {
  rmSync(destinationRoot, { recursive: true, force: true })
})

const compileSource = (
  name: string,
  text: string,
  overrides: Partial<Driver.CompileRequest> = {},
): Effect.Effect<Driver.Outcome, Driver.SourceResolutionFailed> =>
  Driver.compile({
    compilation: {
      root: SourceFile.make('memory/driver', ascii(text)),
    },
    toolchain,
    profile: 'release',
    destination: join(destinationRoot, name),
    ...overrides,
  }).pipe(Effect.provide(SourceResolver.empty))

const expectedPhases = [
  'closure',
  'declaration-index',
  'name-resolution',
  'elaboration',
  'ownership',
  'instance-discovery',
  'target-layout',
  'mir-lowering',
  'backend',
  'object',
  'shim',
  'link',
]

it.effect('compiles the nested program to a running executable matching the interpreter', () =>
  Effect.gen(function* () {
    const nested = corpus.find((program) => program.name === 'nested')
    assert.notStrictEqual(nested, undefined)
    if (nested === undefined) return
    const outcome = yield* compileSource('nested', nested.source)

    assert.strictEqual(outcome._tag, 'Compiled')
    if (outcome._tag !== 'Compiled') return
    assert.strictEqual(outcome.target.kind, 'Native')
    assert.strictEqual(existsSync(outcome.executable), true)
    const run = spawnSync(outcome.executable, [], { encoding: 'utf8' })
    const interpreted = Analysis.evaluate(
      yield* Analysis.ofSource('memory/driver', ascii(nested.source)),
    )
    assert.strictEqual(interpreted._tag, 'Completed')
    if (interpreted._tag !== 'Completed') return
    assert.strictEqual(run.status, interpreted.result.value)
  }),
)

it.effect('compiles a three-module call chain to native execution matching the interpreter', () =>
  Effect.gen(function* () {
    const sources = new Map([
      [
        'app/Main',
        ascii('import library.Answer { answer }\npub fn main() -> I32 { return answer() }'),
      ],
      [
        'library/Answer',
        ascii('import values.Number { two }\npub fn answer() -> I32 { return I32.add(40, two()) }'),
      ],
      ['values/Number', ascii('pub fn two() -> I32 { return 2 }')],
    ])
    const root = sources.get('app/Main')
    if (root === undefined) return assert.fail('expected app/Main fixture')
    const imports = new Map([...sources].filter(([name]) => name !== 'app/Main'))
    const layer = SourceResolver.memory(imports)
    const outcome = yield* Driver.compile({
      compilation: { root: SourceFile.make('app/Main', root) },
      toolchain,
      profile: 'release',
      destination: join(destinationRoot, 'cross-module'),
    }).pipe(Effect.provide(layer))
    const interpreted = Analysis.evaluate(
      yield* Analysis.make({ root: SourceFile.make('app/Main', root) }).pipe(Effect.provide(layer)),
    )
    assert.strictEqual(outcome._tag, 'Compiled')
    assert.strictEqual(interpreted._tag, 'Completed')
    if (outcome._tag !== 'Compiled' || interpreted._tag !== 'Completed') return
    const run = spawnSync(outcome.executable, [], { encoding: 'utf8' })
    assert.strictEqual(run.status, interpreted.result.value)
  }),
)

it.effect('reports every phase in order with counts and totals', () =>
  Effect.gen(function* () {
    const outcome = yield* compileSource('report', 'pub fn main() -> I32 { return 42 }')

    assert.strictEqual(outcome._tag, 'Compiled')
    if (outcome._tag !== 'Compiled') return
    assert.deepEqual(
      outcome.report.map((entry) => entry.phase),
      expectedPhases,
    )
    for (const entry of outcome.report) {
      assert.isAtLeast(entry.elapsedMs, 0, entry.phase)
      assert.isAtLeast(entry.outputs, 0, entry.phase)
      assert.isAbove(entry.heapBytes, 0, entry.phase)
    }
    const closure = outcome.report.at(0)
    assert.strictEqual(closure?.inputs, 1)
    assert.strictEqual(closure?.outputs, 1)
  }),
)

it.effect('routes emission through an injected backend service', () =>
  Effect.gen(function* () {
    let emissions = 0
    const spy: Backend.Backend = {
      _tag: 'Backend',
      name: 'Spy LLVM',
      targets: Backend.LlvmBackend.targets,
      emit: (program, request) => {
        emissions += 1
        return Backend.LlvmBackend.emit(program, request)
      },
    }
    const outcome = yield* compileSource('injected-backend', 'pub fn main() -> I32 { return 42 }', {
      backend: spy,
    })

    assert.strictEqual(emissions, 1)
    assert.strictEqual(outcome._tag, 'Compiled')
  }),
)

it.effect('gates source rejection and operational resolution failure before backend work', () =>
  Effect.gen(function* () {
    let emissions = 0
    const spy: Backend.Backend = {
      _tag: 'Backend',
      name: 'Gate Spy',
      targets: Backend.LlvmBackend.targets,
      emit: (program, request) => {
        emissions += 1
        return Backend.LlvmBackend.emit(program, request)
      },
    }
    const rejected = yield* compileSource('rejected', 'pub fn main() -> Mystery { return 42 }', {
      backend: spy,
    })
    assert.strictEqual(rejected._tag, 'Rejected')
    assert.strictEqual(emissions, 0)
    assert.strictEqual(
      rejected.report.some((entry) => entry.phase === 'target-layout'),
      false,
    )

    const resolver = Layer.succeed(SourceResolver.SourceResolver, {
      resolve: (module: string) =>
        Effect.fail(
          new SourceResolver.SourceResolverError({
            operation: 'test.resolve',
            module,
            message: `cannot read ${module}`,
            reason: { _tag: 'WrappedFailure', cause: new Error(module) },
          }),
        ),
    })
    const failed = yield* Effect.result(
      Driver.compile({
        compilation: {
          root: SourceFile.make(
            'memory/driver',
            ascii('import unreadable\npub fn main() -> I32 { return 42 }'),
          ),
        },
        toolchain,
        profile: 'release',
        destination: join(destinationRoot, 'resolution-failed'),
        backend: spy,
      }).pipe(Effect.provide(resolver)),
    )
    assert.strictEqual(failed._tag, 'Failure')
    if (failed._tag === 'Failure') {
      assert.strictEqual(failed.failure._tag, 'SourceResolutionFailed')
      assert.deepEqual(
        failed.failure.failures.map((failure) => failure.module),
        ['unreadable'],
      )
      assert.strictEqual(
        failed.failure.report.some((entry) => entry.phase === 'target-layout'),
        false,
      )
    }
    assert.strictEqual(emissions, 0)
  }),
)

it.effect('surfaces a missing entry as a closed outcome without invoking the toolchain', () =>
  Effect.gen(function* () {
    const outcome = yield* compileSource('no-entry', 'pub fn answer() -> I32 { return 42 }')

    assert.strictEqual(outcome._tag, 'NoEntry')
    if (outcome._tag !== 'NoEntry') return
    assert.strictEqual(outcome.reason, 'MissingEntry')
    assert.strictEqual(
      outcome.report.some((entry) => entry.phase === 'object'),
      false,
    )
  }),
)

it.effect('names the failing native stage with command provenance', () =>
  Effect.gen(function* () {
    const outcome = yield* compileSource('bad-toolchain', 'pub fn main() -> I32 { return 42 }', {
      toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/nonexistent/clang' }),
    })

    assert.strictEqual(outcome._tag, 'Failed')
    if (outcome._tag !== 'Failed') return
    assert.strictEqual(outcome.stage, 'object')
    assert.strictEqual(outcome.failure.planned.command, '/nonexistent/clang')
  }),
)

it.effect(
  'keeps the interpreter and native execution in agreement across the corpus',
  () =>
    Effect.gen(function* () {
      for (const program of corpus) {
        const interpreted = Analysis.evaluate(
          yield* Analysis.ofSource('memory/driver', ascii(program.source)),
        )
        const outcome = yield* compileSource(`corpus-${program.name}`, program.source)

        if (program.expected._tag === 'UnavailableEntry') {
          assert.strictEqual(outcome._tag, 'NoEntry', program.name)
          continue
        }

        if (outcome._tag === 'Rejected') {
          assert.strictEqual(program.expected._tag, 'Trap', program.name)
          assert.strictEqual(outcome.diagnostics.length > 0, true, program.name)
          continue
        }

        assert.strictEqual(outcome._tag, 'Compiled', program.name)
        if (outcome._tag !== 'Compiled') continue

        if (program.expected._tag === 'Completes') {
          assert.strictEqual(interpreted._tag, 'Completed', program.name)
          const run = spawnSync(outcome.executable, [], { encoding: 'utf8' })
          assert.strictEqual(
            run.status,
            interpreted._tag === 'Completed' ? interpreted.result.value : -1,
            `differential divergence on ${program.name}: interpreter ${
              interpreted._tag === 'Completed' ? interpreted.result.value : interpreted._tag
            }, native ${run.status}`,
          )
          continue
        }

        if (program.expected._tag === 'Trap') {
          const run = spawnSync(outcome.executable, [], { encoding: 'utf8' })
          assert.strictEqual(
            run.signal !== null || (run.status !== null && run.status !== 0),
            true,
            `differential divergence on ${program.name}: interpreter trapped, native exited ${run.status}`,
          )
        }

        // RecursiveCycle: the program must compile; native unbounded recursion is not executed.
      }
    }),
  60000,
)

it.effect('stops unsupported and WebAssembly targets before MIR or native tools', () =>
  Effect.gen(function* () {
    for (const target of ['mips-unknown-none', 'wasm32-unknown-unknown']) {
      const outcome = yield* compileSource(
        `target-${target}`,
        'pub fn main() -> I32 { return 42 }',
        {
          compilation: {
            root: SourceFile.make('memory/driver', ascii('pub fn main() -> I32 { return 42 }')),
            target,
          },
        },
      )

      assert.strictEqual(outcome._tag, 'TargetFailed')
      assert.strictEqual(
        outcome.report.some((entry) => entry.phase === 'mir-lowering'),
        false,
      )
      assert.strictEqual(
        outcome.report.some((entry) => entry.phase === 'object'),
        false,
      )
    }
  }),
)
