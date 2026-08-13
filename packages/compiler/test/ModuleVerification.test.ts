import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as SourceResolver from '../src/SourceResolver.js'
import { corpus } from './support/corpus.js'

/**
 * Both backends verify every module they emit, so every existing backend test is already a
 * verifier test. What is left to pin here is that the in-process LLVM verifier agrees with the
 * tool it stands in for: `opt -passes=verify`, the command that found #130. A verifier nobody has
 * checked against the real one is only a claim.
 */

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/**
 * `opt` ships in LLVM's tools package rather than alongside `clang`, so a machine that compiles
 * Silk programs does not necessarily have it on `PATH`. Look where the distributions put it before
 * concluding it is absent.
 */
const opt = [
  process.env.SILK_TEST_OPT,
  'opt',
  '/opt/homebrew/opt/llvm/bin/opt',
  '/usr/local/opt/llvm/bin/opt',
  ...['21', '20', '19', '18', '17', '16'].map((version) => `/usr/lib/llvm-${version}/bin/opt`),
].find(
  (candidate) =>
    candidate !== undefined &&
    spawnSync(candidate, ['--version'], { encoding: 'utf8' }).status === 0,
)

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-module-verification-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

it.effect(
  'emits corpus modules that opt also accepts',
  () =>
    Effect.gen(function* () {
      if (opt === undefined) {
        // A cross-check that turns itself off when its reference implementation is missing
        // reports success in exactly the situation where it verified nothing — the failure mode
        // this file exists to remove. A contributor without LLVM's tools still gets a useful
        // suite; CI gets a red build, answered by installing LLVM rather than by a quieter test.
        if (process.env.CI !== undefined) {
          return assert.fail(
            'opt was not found in CI, so the LLVM verifier cross-check cannot run. Install LLVM in the workflow, or point SILK_TEST_OPT at the binary.',
          )
        }
        console.log('opt was not found; skipping the LLVM verifier cross-check')
        return
      }
      console.log(`cross-checking the LLVM verifier against ${opt}`)
      let emitted = 0
      const rejected: Array<string> = []
      for (const program of corpus) {
        const artifact = yield* Analysis.ofSourceRealized(
          'memory/verify',
          ascii(program.source),
        ).pipe(
          Effect.flatMap((snapshot) => Analysis.codegen(snapshot, { mode: 'release' })),
          Effect.provide(SourceResolver.empty),
          Effect.result,
        )
        // A corpus entry pinned as an unavailable entry, or one whose analysis is deliberately
        // invalid, never reaches a backend. Nothing to verify for those.
        if (artifact._tag !== 'Success') continue
        emitted += 1
        const path = join(destinationRoot, `${program.name}.bc`)
        writeFileSync(path, artifact.success.bitcode)
        const verified = spawnSync(opt, ['-passes=verify', '-disable-output', path], {
          encoding: 'utf8',
        })
        if (verified.status !== 0) rejected.push(`${program.name}:\n${verified.stderr}`)
      }
      assert.isAbove(emitted, 0)
      assert.deepEqual(rejected, [])
    }),
  600_000,
)
