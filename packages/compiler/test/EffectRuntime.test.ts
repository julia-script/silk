import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Intrinsic from '../src/Intrinsic.js'
import * as Type from '../src/Type.js'
import * as Projections from './support/projections.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))
const flattenPrelude = `effect fn inner(value: i32) -> i32 { return value * 2 }
effect fn outer(value: i32) -> Effect<i32> { return inner(value) }`
const flattenSource = `import silk.effect { Effect }
${flattenPrelude}
pub fn main() -> i32 {
  let nested = outer(21)
  let flattened = Effect.flatten(move nested)
  return run flattened
}`
const flattenRowsSource = `import silk.effect { Effect }
struct Outer { code: i32 }
struct Inner { code: i32 }
service Clock {}
service Meter {}
effect fn inner() -> i32 ! Inner ? &Clock | &Meter { return 21 }
effect fn outer() -> Effect<i32 ! Inner ? &Clock | &Meter> ! Outer ? &Clock { return inner() }
pub fn main() -> i32 {
  let nested = outer()
  let flattened = Effect.flatten(move nested)
  return 0
}`

it.effect('unions both failure rows and both requirement rows through flatten', () =>
  Effect.gen(function* () {
    const module = 'effect-runtime/flatten-rows'
    const snapshot = yield* Analysis.ofSourceRealized(module, ascii(flattenRowsSource))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const encoded = Analysis.expressionsOf(snapshot, module).flatMap((expression) =>
      expression._tag === 'Call' && expression.type._tag === 'Available'
        ? [Type.encode(expression.type.type)]
        : [],
    )
    assert.deepEqual(encoded, [
      `Effect<i32 ! ${module}.Inner ? &${module}.Clock | &${module}.Meter>`,
      `Effect<Effect<i32 ! ${module}.Inner ? &${module}.Clock | &${module}.Meter> ! ${module}.Outer ? &${module}.Clock>`,
      `Effect<i32 ! ${module}.Inner | ${module}.Outer ? &${module}.Clock | &${module}.Meter>`,
    ])
  }),
)

it.effect('resolves flatten through the ordinary declaration path without an intrinsic', () =>
  Effect.gen(function* () {
    const module = 'effect-runtime/flatten-declaration'
    const snapshot = yield* Analysis.ofSourceRealized(module, ascii(flattenSource))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const occurrence = Analysis.semanticOccurrenceAt(
      snapshot,
      module,
      flattenSource.indexOf('Effect.flatten') + 'Effect.'.length,
    )
    assert.strictEqual(occurrence?.role, 'Value')
    assert.strictEqual(occurrence?.declaration?.module, 'silk/effect')
    assert.include(
      occurrence === undefined
        ? ''
        : (Analysis.occurrencePresentation(snapshot, module, occurrence)?.text ?? ''),
      'pub effect fn flatten',
    )

    const constructed = (Projections.hirOf(snapshot, module)?.functions ?? []).flatMap((fn) =>
      fn.statements.flatMap((statement) =>
        statement._tag === 'Bind' && statement.initializer._tag === 'EffectConstruct'
          ? [
              {
                module: statement.initializer.target.module,
                name: statement.initializer.target.name,
              },
            ]
          : [],
      ),
    )
    assert.deepEqual(constructed, [
      { module, name: 'outer' },
      { module: 'silk/effect', name: 'Effect.flatten' },
    ])

    const intrinsics = new Set(
      (snapshot.semanticOccurrences.modules.get(module)?.occurrences ?? []).flatMap((candidate) =>
        candidate.resolution._tag === 'Available' &&
        candidate.resolution.identity._tag === 'IntrinsicOperationIdentity'
          ? [`${candidate.resolution.identity.id.actor}.${candidate.resolution.identity.id.name}`]
          : [],
      ),
    )
    assert.deepEqual([...intrinsics], ['Intrinsic.i32Multiply'])

    const catalog = Intrinsic.all().flatMap((actor) =>
      actor.operations.map((operation) => operation.spelling),
    )
    assert.notInclude(catalog, 'effectResult')
    assert.notInclude(catalog, 'flatten')
  }),
)
