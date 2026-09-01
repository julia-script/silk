import { assert, it } from '@effect/vitest'
import * as CompilerStdlib from '@silklang/compiler/Stdlib'
import * as Effect from 'effect/Effect'
import * as Doctest from '../src/Doctest.js'
import * as Example from '../src/Example.js'
import * as Json from '../src/Json.js'
import * as Report from '../src/Report.js'
import * as Stdlib from '../src/Stdlib.js'
import { documentation as stdlibDocumentation } from './support/doctestStdlibDocumentation.js'

/**
 * Every example the standard library documents, opted out or not.
 *
 * The set is pinned rather than counted so an opt-out cannot be added quietly. A new
 * ```` ```silk,ignore ```` fence fails this test until someone writes it down here, which is the
 * only thing standing between a doctest gate and a gate that skips everything it is given.
 */
const skipped: ReadonlyArray<string> = []

/**
 * The live doctest sweep compiles every stdlib example and is by far this file's dominant cost, so
 * the two tests that need the same live report share one run instead of each paying for their own.
 */
let liveReportOnce: Doctest.Report | undefined
const liveReport = Effect.gen(function* () {
  if (liveReportOnce === undefined) {
    const documentation = yield* stdlibDocumentation
    liveReportOnce = yield* Doctest.run({ documentation, sources: Stdlib.sources })
  }
  return liveReportOnce
})

it.effect(
  'compiles every fenced Silk example in the standard library',
  () =>
    Effect.gen(function* () {
      const report = yield* liveReport

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
      // Compiling an example is a pure function of the collected example, and the live sweep
      // above already compiled every one — the round trip has to prove only that collection reads
      // the same examples out of what `silk doc` writes as out of the live value.
      const roundTripped = Example.collect(parsed)
      assert.isAbove(roundTripped.length, 0)
      assert.deepStrictEqual(roundTripped, Example.collect(documentation))
    }),
  180_000,
)
