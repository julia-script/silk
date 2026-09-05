import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Intrinsic from '../src/Intrinsic.js'
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/** Runtime order and failure behavior live in the native corpus; this fixture proves declaration resolution. */
const zipping3 = `import silk.effect { Effect }
effect fn step(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let pair = run Effect.zip(step(1), step(2))
  let triple = run Effect.zip3(step(3), step(4), step(5))
  return pair.first + triple.third
}`

/** Distinct failure rows and distinct requirement rows on each operand, so both unions show. */
const zipRowsSource = `import silk.effect { Effect }
struct Left { code: i32 }
struct Right { code: i32 }
service Clock {}
service Meter {}
effect fn left() -> i32 ! Left ? &Clock { return 40 }
effect fn right() -> i32 ! Right ? &Meter { return 2 }
pub fn main() -> i32 {
  let zipped = Effect.zip(left(), right())
  return 0
}`

const zip3RowsSource = `import silk.effect { Effect }
struct Left { code: i32 }
struct Middle { code: i32 }
struct Right { code: i32 }
service Clock {}
service Meter {}
service Gauge {}
effect fn left() -> i32 ! Left ? &Clock { return 40 }
effect fn middle() -> i32 ! Middle ? &Meter { return 2 }
effect fn right() -> i32 ! Right ? &Gauge { return 0 }
pub fn main() -> i32 {
  let zipped = Effect.zip3(left(), middle(), right())
  return 0
}`

/** Requirement 6: both failure rows and both requirement rows are unioned. */
it.effect('unions the failure rows and the requirement rows of both zipped Effects', () =>
  Effect.gen(function* () {
    const module = 'zip/rows'
    const snapshot = yield* Analysis.ofSource(module, ascii(zipRowsSource))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const encoded = Analysis.expressionsOf(snapshot, module).flatMap((expression) =>
      expression._tag === 'Call' &&
      expression.type._tag === 'Available' &&
      Type.isEffect(expression.type.type)
        ? [
            {
              success: Type.encode(expression.type.type.success),
              failures: Type.failureMembers(expression.type.type).map((type) => Type.encode(type)),
              requirements: Type.requirementMembers(expression.type.type).map((requirement) =>
                Type.encodeRequirement(requirement),
              ),
            },
          ]
        : [],
    )
    // The zip call itself is encoded first, then its two operands.
    assert.deepEqual(encoded, [
      {
        success: 'silk/effect.Pair<i32, i32>',
        failures: [`${module}.Left`, `${module}.Right`],
        requirements: [`&${module}.Clock`, `&${module}.Meter`],
      },
      { success: 'i32', failures: [`${module}.Left`], requirements: [`&${module}.Clock`] },
      { success: 'i32', failures: [`${module}.Right`], requirements: [`&${module}.Meter`] },
    ])
  }),
)

it.effect('unions all three failure rows and all three requirement rows through zip3', () =>
  Effect.gen(function* () {
    const module = 'zip/rows3'
    const snapshot = yield* Analysis.ofSource(module, ascii(zip3RowsSource))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const encoded = Analysis.expressionsOf(snapshot, module).flatMap((expression) =>
      expression._tag === 'Call' &&
      expression.type._tag === 'Available' &&
      Type.isEffect(expression.type.type)
        ? [
            {
              success: Type.encode(expression.type.type.success),
              failures: Type.failureMembers(expression.type.type).map((type) => Type.encode(type)),
              requirements: Type.requirementMembers(expression.type.type).map((requirement) =>
                Type.encodeRequirement(requirement),
              ),
            },
          ]
        : [],
    )
    assert.strictEqual(encoded.length, 4)
    assert.deepEqual(encoded[0], {
      success: 'silk/effect.Triple<i32, i32, i32>',
      failures: [`${module}.Left`, `${module}.Middle`, `${module}.Right`],
      requirements: [`&${module}.Clock`, `&${module}.Gauge`, `&${module}.Meter`],
    })
  }),
)

/**
 * Requirement 7 and the closed combinator list: both combinators are ordinary shipped Silk
 * declarations. Nothing about them is selected from their names, and no intrinsic is registered.
 */
it.effect('resolves zip and zip3 through the ordinary declaration path without an intrinsic', () =>
  Effect.gen(function* () {
    const module = 'zip/declaration'
    const snapshot = yield* Analysis.ofSource(module, ascii(zipping3))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    for (const [name, offset] of [
      ['zip', zipping3.indexOf('Effect.zip(') + 'Effect.'.length],
      ['zip3', zipping3.indexOf('Effect.zip3') + 'Effect.'.length],
    ] as const) {
      const occurrence = Analysis.semanticOccurrenceAt(snapshot, module, offset)
      assert.strictEqual(occurrence?.role, 'Value', name)
      assert.strictEqual(occurrence?.declaration?.module, 'silk/effect', name)
      assert.include(
        occurrence === undefined
          ? ''
          : (Analysis.occurrencePresentation(snapshot, module, occurrence)?.text ?? ''),
        `pub effect<'env> fn ${name}`,
        name,
      )
    }

    const catalog = Intrinsic.all().flatMap((actor) =>
      actor.operations.map((operation) => operation.spelling),
    )
    assert.notInclude(catalog, 'effectResult')
    assert.notInclude(catalog, 'zip')
    assert.notInclude(catalog, 'zip3')
  }),
)
