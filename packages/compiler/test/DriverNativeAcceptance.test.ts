import { spawnSync } from 'node:child_process'
import { existsSync, mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import { nativeCorpus } from './support/corpus.js'
import * as Driver from './support/TestDriver.js'

const defaultClang = (): string => {
  if (existsSync('/opt/homebrew/opt/llvm/bin/clang')) return '/opt/homebrew/opt/llvm/bin/clang'
  if (existsSync('/usr/local/opt/llvm/bin/clang')) return '/usr/local/opt/llvm/bin/clang'
  return 'clang'
}

const clang = process.env.SILK_TEST_CLANG ?? defaultClang()
const toolchain: NativeToolchain.Toolchain = Object.freeze({
  _tag: 'Toolchain',
  clang,
  shimCache: NativeToolchain.makeShimCache(),
})

// UTF-8, not charCodeAt: corpus programs may carry non-ASCII literals, and for ASCII sources the
// bytes are identical.
const encoder = new TextEncoder()
const ascii = (value: string): Uint8Array => encoder.encode(value)

const runCompiled = Effect.fnUntraced(function* (
  path: string,
  nativeEnvironment: Readonly<Record<string, string>> | undefined,
) {
  return yield* Effect.sync(() =>
    spawnSync(path, [], {
      encoding: 'utf8',
      ...(nativeEnvironment === undefined ? {} : { env: { ...process.env, ...nativeEnvironment } }),
    }),
  )
})

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-driver-native-acceptance-'))
afterAll(() => {
  rmSync(destinationRoot, { recursive: true, force: true })
})

const compileSource = (
  name: string,
  text: string,
  imports?: Readonly<Record<string, string>>,
): Effect.Effect<Driver.Outcome, Driver.SourceResolutionFailed | NativeToolchain.ToolchainError> =>
  Driver.compile({
    compilation: {
      root: SourceFile.make('memory/driver', ascii(text)),
    },
    toolchain,
    profile: 'release',
    destination: join(destinationRoot, name),
  }).pipe(
    Effect.provide(
      imports === undefined
        ? SourceResolver.empty
        : SourceResolver.memory(
            new Map(
              Object.entries(imports).map(([module, source]) => [module, ascii(source)] as const),
            ),
          ),
    ),
  )

/**
 * `SILK_NATIVE_SHARD=k/n` selects every n-th corpus case starting at k (1-based), letting CI run
 * the corpus as a job matrix instead of one serial sweep. Unset runs everything.
 */
const shard = /^([1-9]\d*)\/([1-9]\d*)$/.exec(process.env.SILK_NATIVE_SHARD ?? '')
const shardedCorpus =
  shard === null
    ? nativeCorpus
    : nativeCorpus.filter((_, index) => index % Number(shard[2]) === Number(shard[1]) - 1)

it.each(shardedCorpus)(
  'runs the native corpus case $name',
  async (program) => {
    const selected = process.env.SILK_NATIVE_CORPUS_CASE

    await Effect.gen(function* () {
      if (selected !== undefined && program.name !== selected) return
      const snapshot = yield* Analysis.ofSourceRealized('memory/driver', ascii(program.source))
      assert.strictEqual(
        snapshot.mir._tag,
        'Available',
        `${program.name}: ${Analysis.diagnostics(snapshot)
          .map((diagnostic) => diagnostic.code)
          .join(',')}`,
      )
      if (snapshot.mir._tag !== 'Available') return
      const interpreted = Analysis.evaluate(snapshot)
      const outcome = yield* compileSource(
        `corpus-${program.name}`,
        program.nativeSource ?? program.source,
        program.nativeImports,
      )

      if (program.expected._tag === 'UnavailableEntry') {
        assert.strictEqual(outcome._tag, 'NoEntry', program.name)
        return
      }

      if (outcome._tag === 'Rejected') {
        assert.strictEqual(program.expected._tag, 'Trap', program.name)
        assert.strictEqual(outcome.diagnostics.length > 0, true, program.name)
        return
      }

      assert.strictEqual(
        outcome._tag,
        'Compiled',
        outcome._tag === 'BackendFailed'
          ? `${program.name}: ${outcome.error.message}\n${JSON.stringify(outcome.error.reason)}`
          : program.name,
      )
      if (outcome._tag !== 'Compiled') return

      if (program.expected._tag === 'Completes') {
        assert.strictEqual(interpreted._tag, 'Completed', program.name)
        const run = yield* runCompiled(outcome.path, program.nativeEnvironment)
        if (program.nativeStdout !== undefined)
          assert.strictEqual(run.stdout, program.nativeStdout, program.name)
        const nativeStatus = run.status === null ? null : BigInt(run.status)
        // POSIX exposes only the low unsigned byte of a process exit value.
        const interpretedStatus =
          interpreted._tag === 'Completed' ? interpreted.result.value & 0xffn : -1n
        assert.strictEqual(
          nativeStatus,
          interpretedStatus,
          `differential divergence on ${program.name}: interpreter ${
            interpreted._tag === 'Completed' ? interpreted.result.value : interpreted._tag
          }, native ${run.status}`,
        )
        return
      }

      if (program.expected._tag === 'Trap') {
        const run = yield* runCompiled(outcome.path, program.nativeEnvironment)
        assert.strictEqual(
          run.signal !== null || (run.status !== null && run.status !== 0),
          true,
          `differential divergence on ${program.name}: interpreter trapped, native exited ${run.status}`,
        )
      }
    }).pipe(Effect.runPromise)
  },
  1_500_000,
)
it.effect(
  'links and runs the native system and monotonic clock ABI',
  () =>
    Effect.gen(function* () {
      const outcome = yield* compileSource(
        'native-clocks',
        `import silk.effect as Effect
import silk.monotonic_clock as MonotonicClock
import silk.os_monotonic_clock as OsMonotonicClock
import silk.os_system_clock as OsSystemClock
import silk.system_clock as SystemClock
pub fn main() -> i32 {
  let mut system = OsSystemClock.make()
  let systemNow = run Effect.provideMut(SystemClock.now(), &mut system)
  let systemResolution = run Effect.provideMut(SystemClock.getResolution(), &mut system)
  let systemFraction = SystemClock.nanoseconds(&systemNow)
  if systemFraction < 0 { return 1 }
  if systemFraction >= 1000000000 { return 2 }
  if systemResolution == 0 { return 3 }

  let mut monotonic = OsMonotonicClock.make()
  let mark = run Effect.provideMut(MonotonicClock.now(), &mut monotonic)
  let monotonicResolution = run Effect.provideMut(
    MonotonicClock.getResolution(),
    &mut monotonic
  )
  let monotonicFraction = SystemClock.nanoseconds(&mark)
  if monotonicFraction < 0 { return 4 }
  if monotonicFraction >= 1000000000 { return 5 }
  if monotonicResolution == 0 { return 6 }
  run Effect.provideMut(MonotonicClock.waitUntil(move mark), &mut monotonic)
  run Effect.provideMut(MonotonicClock.waitFor(1), &mut monotonic)
  return 42
}`,
      )
      assert.strictEqual(
        outcome._tag,
        'Compiled',
        outcome._tag === 'BackendFailed'
          ? `${outcome.error.message}\n${JSON.stringify(outcome.error.reason)}`
          : outcome._tag,
      )
      if (outcome._tag !== 'Compiled') return
      const run = yield* runCompiled(outcome.path, undefined)
      assert.strictEqual(run.signal, null)
      assert.strictEqual(run.status, 42)
    }),
  120_000,
)
