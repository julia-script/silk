import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Path from 'effect/Path'
import * as Stream from 'effect/Stream'
import { ChildProcess } from 'effect/unstable/process'

/**
 * SEM0103 is part of the frontend's stable contract, so its reports must be byte-identical across
 * fresh processes: same codes, messages, spans, related provenance, and ordering. The fixture
 * covers a nested callable-bearing aggregate and one generic wrapper specialized twice, which is
 * where an unstable iteration order or presentation-dependent key would first show up.
 */

interface Report {
  readonly diagnostics: ReadonlyArray<{
    readonly code: string
    readonly message: string
    readonly span: { readonly sourceId: string; readonly start: number; readonly end: number }
    readonly related: ReadonlyArray<{
      readonly label: string
      readonly sourceId: string
      readonly start: number
      readonly end: number
    }>
  }>
  readonly layout: string
  readonly mir: string
}

const collectText = Stream.runFold(
  () => '',
  (text: string, chunk: string) => text + chunk,
)

const runFixture = Effect.fnUntraced(function* (fixture: string) {
  const handle = yield* ChildProcess.make(process.execPath, [fixture], { stdin: 'ignore' })
  const [code, stdout, stderr] = yield* Effect.all(
    [
      handle.exitCode,
      handle.stdout.pipe(Stream.decodeText(), collectText),
      handle.stderr.pipe(Stream.decodeText(), collectText),
    ],
    { concurrency: 'unbounded' },
  )
  return { code, stdout, stderr }
})

it.effect('keeps SEM0103 reports byte-identical across fresh processes', () =>
  Effect.gen(function* () {
    const path = yield* Path.Path
    const fixture = yield* path.fromFileUrl(
      new URL('./fixtures/stored-callable-determinism.mjs', import.meta.url),
    )
    const first = yield* runFixture(fixture)
    const second = yield* runFixture(fixture)

    assert.strictEqual(first.code, 0, first.stderr)
    assert.strictEqual(second.code, 0, second.stderr)
    assert.strictEqual(first.stdout, second.stdout)

    const report = JSON.parse(first.stdout) as Report
    // Two declared-field violations and two specializations of one generic wrapper.
    assert.deepEqual(
      report.diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0103', 'SEM0103', 'SEM0103', 'SEM0103'],
      report.diagnostics.map((diagnostic) => diagnostic.message).join('\n'),
    )
    // Merge order is span order: report ordering is a published fact, not an accident.
    const positions = report.diagnostics.map((diagnostic) => diagnostic.span.start)
    assert.deepEqual(
      [...positions].sort((left, right) => left - right),
      positions,
    )
    // Both wrapper specializations report at their own call sites in the user source.
    for (const diagnostic of report.diagnostics) {
      assert.strictEqual(diagnostic.span.sourceId, 'fixture/StoredCallable')
    }
    const related = report.diagnostics.flatMap((diagnostic) => diagnostic.related)
    assert.deepEqual(
      related.map((span) => span.label),
      ['constructed here', 'constructed here'],
    )
    // The fence holds in the same breath: no layout or MIR is realized for the rejected program.
    assert.strictEqual(report.layout, 'Unavailable')
    assert.strictEqual(report.mir, 'Unavailable')
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)
