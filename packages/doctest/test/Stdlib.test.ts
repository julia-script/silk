import { assert, it } from '@effect/vitest'
import * as CompilerStdlib from '@silk-effect/compiler/Stdlib'
import * as Json from '@silk-effect/documentation/Json'
import * as Effect from 'effect/Effect'
import * as Doctest from '../src/Doctest.js'
import * as Report from '../src/Report.js'
import * as Stdlib from '../src/Stdlib.js'
import { documentation as stdlibDocumentation } from './support/stdlibDocumentation.js'

/**
 * Every example the standard library documents, opted out or not.
 *
 * The set is pinned rather than counted so an opt-out cannot be added quietly. A new
 * ```` ```silk,ignore ```` fence fails this test until someone writes it down here, which is the
 * only thing standing between a doctest gate and a gate that skips everything it is given.
 */
const skipped: ReadonlyArray<string> = []

it.effect(
  'compiles every fenced Silk example in the standard library',
  () =>
    Effect.gen(function* () {
      const documentation = yield* stdlibDocumentation
      const report = yield* Doctest.run({ documentation, sources: Stdlib.sources })

      assert.isAbove(
        report.collected,
        0,
        'the standard library must carry fenced Silk examples for this gate to mean anything',
      )
      assert.isAbove(
        report.passed,
        0,
        'at least one standard-library example must actually be compiled, or this gate compiles nothing',
      )
      assert.deepStrictEqual(report.failed, 0, Report.render(report))
      assert.deepStrictEqual(
        report.results
          .filter((result) => result.outcome._tag === 'Skipped')
          .map(
            (result) =>
              `${result.example.owner.module}::${result.example.owner.declaration ?? '<module>'}`,
          ),
        skipped,
      )
    }),
  180_000,
)

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
      const parsed: unknown = JSON.parse(Json.encode(documentation))
      const live = yield* Doctest.run({ documentation, sources: Stdlib.sources })
      const roundTripped = yield* Doctest.run({
        documentation: parsed,
        sources: Stdlib.sources,
      })
      assert.deepStrictEqual(
        {
          collected: roundTripped.collected,
          passed: roundTripped.passed,
          skipped: roundTripped.skipped,
          failed: roundTripped.failed,
        },
        {
          collected: live.collected,
          passed: live.passed,
          skipped: live.skipped,
          failed: live.failed,
        },
      )
    }),
  180_000,
)
