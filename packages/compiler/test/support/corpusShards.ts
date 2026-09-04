import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, describe, it } from '@effect/vitest'
import * as Console from 'effect/Console'
import * as Effect from 'effect/Effect'
import { llvmToolchain } from '../../../../test/support/llvmToolchain.js'
import * as Analysis from '../../src/Analysis.js'
import * as SourceResolver from '../../src/SourceResolver.js'
import { corpus } from './corpus.js'

/**
 * The pinned corpus sweeps used to run as one test each, which serialized minutes of work inside
 * a single vitest worker while the rest of the machine sat idle. Each sweep now registers from
 * several thin shard files — vitest parallelizes per file — and one `it` per program, so a rank
 * report names the exact corpus entry that got slow.
 */

// UTF-8, not charCodeAt: corpus programs may carry non-ASCII literals, and for ASCII sources the
// bytes are identical.
const encoder = new TextEncoder()
const ascii = (value: string): Uint8Array => encoder.encode(value)

/** Every `of`-th pinned corpus program starting at `shard` (1-based). */
const corpusShard = (shard: number, of: number): typeof corpus =>
  corpus.filter((_, index) => index % of === shard - 1)

/** How many shard files each sweep registers; the shard files pass their own 1-based index. */
export const corpusShardCount = 4

/**
 * Registers the `opt -passes=verify` cross-check for one corpus shard.
 *
 * Both backends verify every module they emit, so every existing backend test is already a
 * verifier test. What is pinned here is that the in-process LLVM verifier agrees with the tool it
 * stands in for: `opt -passes=verify`, the command that found #130. A verifier nobody has checked
 * against the real one is only a claim.
 */
export const moduleVerificationShard = (shard: number): void => {
  const toolchain = llvmToolchain(
    ['opt'],
    `the LLVM verifier cross-check (shard ${shard})`,
    (message) => Effect.runSync(Console.log(message)),
  )
  const destinationRoot = mkdtempSync(join(tmpdir(), `silk-module-verification-${shard}-`))
  afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

  describe(`opt verifier agreement (shard ${shard}/${corpusShardCount})`, () => {
    let emitted = 0
    it.each(corpusShard(shard, corpusShardCount))(
      'emits a module for $name that opt also accepts',
      async (program) => {
        if (toolchain.unavailable()) return
        const opt = toolchain.command('opt')
        await Effect.gen(function* () {
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
          if (artifact._tag !== 'Success') return
          emitted += 1
          const path = join(destinationRoot, `${program.name}.bc`)
          writeFileSync(path, artifact.success.bitcode)
          const verified = spawnSync(opt, ['-passes=verify', '-disable-output', path], {
            encoding: 'utf8',
          })
          assert.strictEqual(verified.status, 0, `${program.name}:\n${verified.stderr}`)
        }).pipe(Effect.runPromise)
      },
      120_000,
    )
    // Registration order is execution order within the file, so this runs after the sweep: a
    // shard where nothing was emitted is a gate that verified nothing, not a green shard.
    it('emitted at least one module for opt to check', () => {
      if (toolchain.unavailable()) return
      assert.isAbove(emitted, 0)
    })
  })
}
