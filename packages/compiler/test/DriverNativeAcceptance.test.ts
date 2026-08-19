import { spawnSync } from 'node:child_process'
import { existsSync, mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import { nativeCorpus } from './support/corpus.js'

const clang =
  process.env.SILK_TEST_CLANG ??
  (existsSync('/opt/homebrew/opt/llvm/bin/clang')
    ? '/opt/homebrew/opt/llvm/bin/clang'
    : existsSync('/usr/local/opt/llvm/bin/clang')
      ? '/usr/local/opt/llvm/bin/clang'
      : 'clang')
const toolchain: NativeToolchain.Toolchain = Object.freeze({
  _tag: 'Toolchain',
  clang,
  shimCache: NativeToolchain.makeShimCache(),
})

// UTF-8, not charCodeAt: corpus programs may carry non-ASCII literals, and for ASCII sources the
// bytes are identical.
const encoder = new TextEncoder()
const ascii = (value: string): Uint8Array => encoder.encode(value)

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-driver-native-acceptance-'))
afterAll(() => {
  rmSync(destinationRoot, { recursive: true, force: true })
})

const compileSource = (
  name: string,
  text: string,
): Effect.Effect<Driver.Outcome, Driver.SourceResolutionFailed> =>
  Driver.compile({
    compilation: {
      root: SourceFile.make('memory/driver', ascii(text)),
    },
    toolchain,
    profile: 'release',
    destination: join(destinationRoot, name),
  }).pipe(Effect.provide(SourceResolver.empty))

it.effect(
  'keeps the interpreter and native execution in agreement across the corpus',
  () =>
    Effect.gen(function* () {
      for (const program of nativeCorpus) {
        const interpreted = Analysis.evaluate(
          yield* Analysis.ofSourceRealized('memory/driver', ascii(program.source)),
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
          const run = spawnSync(outcome.path, [], { encoding: 'utf8' })
          const nativeStatus = run.status === null ? null : BigInt(run.status)
          assert.strictEqual(
            nativeStatus,
            interpreted._tag === 'Completed' ? interpreted.result.value : -1n,
            `differential divergence on ${program.name}: interpreter ${
              interpreted._tag === 'Completed' ? interpreted.result.value : interpreted._tag
            }, native ${run.status}`,
          )
          continue
        }

        if (program.expected._tag === 'Trap') {
          const run = spawnSync(outcome.path, [], { encoding: 'utf8' })
          assert.strictEqual(
            run.signal !== null || (run.status !== null && run.status !== 0),
            true,
            `differential divergence on ${program.name}: interpreter trapped, native exited ${run.status}`,
          )
        }
      }
    }),
  240_000,
)
