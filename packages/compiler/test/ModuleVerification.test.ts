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

const optAvailable = spawnSync('opt', ['--version'], { encoding: 'utf8' }).status === 0

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-module-verification-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

it.effect(
  'emits corpus modules that opt also accepts',
  () =>
    Effect.gen(function* () {
      if (!optAvailable) {
        // The in-process verifier still ran on every module emitted by every other test; only
        // the cross-check against the reference implementation needs the tool.
        console.log('opt is not on PATH; skipping the LLVM verifier cross-check')
        return
      }
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
        const verified = spawnSync('opt', ['-passes=verify', '-disable-output', path], {
          encoding: 'utf8',
        })
        if (verified.status !== 0) rejected.push(`${program.name}:\n${verified.stderr}`)
      }
      assert.isAbove(emitted, 0)
      assert.deepEqual(rejected, [])
    }),
  600_000,
)
