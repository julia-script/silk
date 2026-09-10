import { assert, it } from '@effect/vitest'
import * as CompilerStdlib from '@silklang/compiler/Stdlib'
import * as Effect from 'effect/Effect'
import * as Example from '../src/Example.js'
import * as Json from '../src/Json.js'
import { documentation as stdlibDocumentation } from './support/doctestStdlibDocumentation.js'

/**
 * Coverage follows the shipped manifest, so a module added to the library is doctested without this
 * file being edited — and a module the documentation build silently drops is caught here rather
 * than by nobody.
 */
it.effect(
  'documents every module of the shipped manifest',
  () =>
    Effect.gen(function* () {
      const documentation = yield* stdlibDocumentation
      assert.deepStrictEqual(
        documentation.modules.map((module) => module.name),
        CompilerStdlib.manifest.map((entry) => entry.module),
      )
    }),
  180_000,
)

/**
 * The workflow's real input is a file, not a live object. Round-tripping through the encoder and
 * `JSON.parse` proves collection reads what `silk doc` writes rather than what the emitter happens
 * to hold in memory.
 */
it.effect(
  'reads the same examples back out of encoded JSON',
  () =>
    Effect.gen(function* () {
      const documentation = yield* stdlibDocumentation
      const parsed = Json.decodeSync(Json.encode(documentation))
      // Compiling an example is a pure function of the collected example, and the live sweep
      // above already compiled every one — the round trip has to prove only that collection reads
      // the same examples out of what `silk doc` writes as out of the live value.
      const roundTripped = Example.collect(parsed)
      assert.isAbove(roundTripped.length, 0)
      assert.deepStrictEqual(roundTripped, Example.collect(documentation))
    }),
  180_000,
)
