import { assert, it } from '@effect/vitest'
import * as Analysis from '@silk-lang/compiler/Analysis'
import * as DocumentationProject from '@silk-lang/documentation/Project'
import * as Effect from 'effect/Effect'
import * as Doctest from '../src/Doctest.js'
import * as Report from '../src/Report.js'
import * as Sources from '../src/Sources.js'

const encoder = new TextEncoder()

const passing = 'pub fn main() -> i32 {\n  return 0\n}\n'
const wrong = 'pub fn main() -> i32 {\n  return missing()\n}\n'

/**
 * A source whose fence sits on a known line, so the reported position can be checked against a real
 * newline count rather than against the same arithmetic the workflow does.
 */
const module = `//! A module.

/// Documented.
///
/// \`\`\`silk
${passing
  .trimEnd()
  .split('\n')
  .map((line) => `/// ${line}`)
  .join('\n')}
/// \`\`\`
pub fn documented() -> i32 { return 0 }
`
/** The opening fence's one-based line, counted independently of the workflow's own arithmetic. */
const fenceLine = module.slice(0, module.indexOf('/// ```')).split('\n').length

const documentationOf = (language: string, code: string, start: number) => ({
  schema: 'silk-documentation',
  experimental: true,
  modules: [
    {
      name: 'project/main',
      sourceId: 'project/main',
      items: [
        {
          id: 'project/main::documented',
          kind: 'Function',
          name: 'documented',
          documentation: {
            blocks: [
              {
                _tag: 'CodeBlock',
                language,
                value: code,
                example: true,
                source: { sourceId: 'project/main', start, end: start + code.length },
              },
            ],
          },
          children: [],
        },
      ],
    },
  ],
})

const sources: Sources.Lookup = (sourceId) =>
  sourceId === 'project/main' ? encoder.encode(module) : undefined

/** The byte offset of the fence in `module`, which is the offset the emitter would record. */
const fenceOffset = encoder.encode(module.slice(0, module.indexOf('/// ```'))).length

const titled = `//! Demonstrates one maintained example.

/// Answers with one.
///
/// # Examples
/// ## Return one from a complete module
/// \`\`\`silk
/// pub fn main() -> i32 { return 1 }
/// \`\`\`
pub fn one() -> i32 { return 1 }
`

it.effect('compiles a titled example collected from authored documentation', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource('doctest/titled', encoder.encode(titled))
    const documentation = DocumentationProject.make(snapshot)
    const report = yield* Doctest.run({ documentation })
    assert.deepStrictEqual(
      {
        collected: report.collected,
        passed: report.passed,
        skipped: report.skipped,
        failed: report.failed,
      },
      { collected: 1, passed: 1, skipped: 0, failed: 0 },
    )
    assert.strictEqual(report.results.at(0)?.example.owner.declaration, 'one')
  }),
)

it.effect('passes an example that compiles as a complete module', () =>
  Effect.gen(function* () {
    const report = yield* Doctest.run({
      documentation: documentationOf('silk', passing, fenceOffset),
      sources,
    })
    assert.deepStrictEqual(
      { collected: report.collected, passed: report.passed, failed: report.failed },
      { collected: 1, passed: 1, failed: 0 },
    )
    assert.isFalse(Doctest.hasFailures(report))
  }),
)

/**
 * The test the whole workflow exists for. A doctest runner nobody has watched fail is a runner that
 * might be compiling nothing, so this asserts the failure, its provenance, and the diagnostic the
 * compiler actually produced — not merely a non-zero count.
 */
it.effect('fails a wrong example and reports its file and its line', () =>
  Effect.gen(function* () {
    const report = yield* Doctest.run({
      documentation: documentationOf('silk', wrong, fenceOffset),
      sources,
    })
    assert.strictEqual(report.failed, 1)
    assert.isTrue(Doctest.hasFailures(report))

    const result = report.results.at(0)
    assert.isDefined(result)
    assert.strictEqual(result.outcome._tag, 'Failed')
    assert.strictEqual(result.example.source.sourceId, 'project/main')
    assert.strictEqual(result.line, fenceLine)
    assert.strictEqual(result.example.owner.module, 'project/main')
    assert.strictEqual(result.example.owner.declaration, 'documented')
    if (result.outcome._tag === 'Failed' && result.outcome.reason._tag === 'Diagnostics')
      assert.isTrue(
        result.outcome.reason.diagnostics.some((diagnostic) => diagnostic.includes('missing')),
        `expected a diagnostic naming the unknown function, saw ${result.outcome.reason.diagnostics.join('; ')}`,
      )
    else assert.fail('a wrong example must fail with compiler diagnostics')

    const rendered = Report.render(report)
    assert.include(rendered, `project/main:${fenceLine}`)
    assert.include(rendered, 'project/main::documented')
    assert.include(rendered, '```silk,ignore')
  }),
)

it.effect('skips an example fenced with the comma-delimited opt-out', () =>
  Effect.gen(function* () {
    const report = yield* Doctest.run({
      documentation: documentationOf('silk,ignore', wrong, fenceOffset),
      sources,
    })
    assert.deepStrictEqual(
      { collected: report.collected, skipped: report.skipped, failed: report.failed },
      { collected: 1, skipped: 1, failed: 0 },
    )
  }),
)

it.effect('fails an example carrying an attribute it does not define', () =>
  Effect.gen(function* () {
    const report = yield* Doctest.run({
      documentation: documentationOf('silk,ignor', passing, fenceOffset),
      sources,
    })
    assert.strictEqual(report.failed, 1)
    const outcome = report.results.at(0)?.outcome
    assert.strictEqual(outcome?._tag, 'Failed')
    if (outcome?._tag === 'Failed') assert.strictEqual(outcome.reason._tag, 'UnknownAttribute')
    assert.include(Report.render(report), 'unknown fence attribute: ignor')
  }),
)

it.effect('reports the position as unavailable rather than guessing a line', () =>
  Effect.gen(function* () {
    const report = yield* Doctest.run({
      documentation: documentationOf('silk', wrong, fenceOffset),
      sources: Sources.empty,
    })
    const result = report.results.at(0)
    assert.isDefined(result)
    assert.isUndefined(result.line)
    assert.include(Report.render(report), 'project/main:<line unavailable>')
  }),
)

it.effect('distinguishes an empty run from a verified one', () =>
  Effect.gen(function* () {
    const report = yield* Doctest.run({ documentation: { modules: [] } })
    assert.strictEqual(report.collected, 0)
    assert.isFalse(Doctest.hasFailures(report))
    assert.include(Report.summary(report), 'no fenced Silk examples were collected')
  }),
)

it('counts a one-based line from a byte offset', () => {
  const bytes = encoder.encode('one\ntwo\nthree\n')
  assert.strictEqual(Sources.lineAt(bytes, 0), 1)
  assert.strictEqual(Sources.lineAt(bytes, 4), 2)
  assert.strictEqual(Sources.lineAt(bytes, 8), 3)
  // A stale offset still names the file it came from rather than losing the report.
  assert.strictEqual(Sources.lineAt(bytes, 10_000), 4)
})
